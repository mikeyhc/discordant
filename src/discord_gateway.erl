-module(discord_gateway).
-behaviour(gen_statem).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0, heartbeat/1, user_id/1, connect/2]).
-export([callback_mode/0, init/1, terminate/3]).
-export([await_connect/3, await_hello/3, await_dispatch/3, connected/3,
         await_ack/3]).

-define(LIBRARY_NAME, <<"discordant">>).

-record(connection, {pid :: pid(),
                     stream_ref :: reference(),
                     ref :: reference()
                    }).
-record(state, {url :: string() | undefined,
                token :: string() | undefined,
                connection :: #connection{} | undefined,
                heartbeat :: discord_heartbeat:ref() | undefined,
                user_id :: binary() | undefined,
                sequence :: integer() | null | undefined,
                session_id :: binary() | undefined,
                log :: file:io_device() | undefined
               }).

%% API functions

-spec start_link() -> any().
start_link() ->
    gen_statem:start_link(?MODULE, [], []).

-spec connect(pid(), string()) -> any().
connect(Pid, Token) ->
    gen_statem:cast(Pid, {connect, Token}).

-spec heartbeat(pid()) -> ok.
heartbeat(Pid) ->
    gen_statem:cast(Pid, heartbeat),
    ok.

-spec user_id(pid()) -> {ok, binary()}.
user_id(Pid) ->
    gen_statem:call(Pid, user_id).

%% gen_statem callbacks

init([]) ->
    gen_statem:cast(self(), reconnect),
    {ok, await_connect, #state{}}.

callback_mode() ->
    state_functions.

terminate({shutdown, reconnect}, _State, Data) ->
    ?LOG_INFO("reconnect requested"),
    disconnect(Data#state.connection, 1001, <<"reconnect">>),
    ?LOG_INFO("removing heartbeat"),
    discord_heartbeat:remove_heartbeat(Data#state.heartbeat);
terminate(disconnected, _State, Data) ->
    disconnect(Data#state.connection, 1001, <<"reconnect">>),
    ?LOG_INFO("removing heartbeat"),
    discord_heartbeat:remove_heartbeat(Data#state.heartbeat);
terminate(Status, State, Data) ->
    ?LOG_INFO("abnormal termination ~p:~p:~p", [Status, State, Data]),
    ok.

%% state callbacks

await_connect(cast, reconnect, State) ->
    case discordant_config:get_value(discord_token) of
        {ok, Token} ->
            ?LOG_INFO("performing reconnect"),
            connect(self(), Token);
        not_found -> ok
    end,
    {keep_state, State};
await_connect(cast, {connect, Token}, State) ->
    connect_(await_hello, State#state{token=Token});
await_connect(cast, _Msg, State) ->
    {keep_state, State, [postpone]}.

await_hello(info, {gun_ws, ConnPid, _StreamRef, {text, Msg}},
            S=#state{connection=#connection{pid=ConnPid}, token=Token}) ->
    Json = decode_msg(Msg, S),
    case Json of
        #{<<"op">> := 10} ->
            ?LOG_INFO("sending identify"),
            send_message(S#state.connection, 2,
                         #{<<"token">> => Token,
                           <<"properties">> => #{
                               <<"$os">> => <<"beam">>,
                               <<"$browser">> => ?LIBRARY_NAME,
                               <<"$device">> => ?LIBRARY_NAME
                              }
                          }),
            {next_state, await_dispatch, handle_ws_message(Json, S)};
        true ->
            {stop, msg_before_hello, S}
    end;
await_hello(_, _, State) ->
    {keep_state, State, [postpone]}.

await_dispatch(info, {gun_ws, ConnPid, _StreamRef, {text, Msg}},
               S=#state{connection=#connection{pid=ConnPid}}) ->
    Json = decode_msg(Msg, S),
    case Json of
        #{<<"op">> := 0} ->
            ?LOG_INFO("connected to discord"),
            {next_state, connected, handle_ws_message(Json, S)};
        #{<<"op">> := 9} ->
            ?LOG_INFO("session invalidated, disconnecting"),
            {stop, disconnected}
    end;
await_dispatch(_, _, S) ->
    {keep_state, S, [postpone]}.

connected(cast, heartbeat,
          S=#state{connection=Connection, sequence=Seq, session_id=Sid}) ->
    ?LOG_INFO("sending heartbeat for session ~p", [Sid]),
    send_message(Connection, 1, Seq),
    {next_state, await_ack, S};
connected({call, From}, user_id, State=#state{user_id=UserId}) ->
    {keep_state, State, [{reply, From, {ok, UserId}}]};
connected(info, {gun_ws, ConnPid, _StreamRef, {text, Msg}},
          S=#state{connection=#connection{pid=ConnPid}, log=Log}) ->
    Json = jsone:decode(Msg),
    ?LOG_DEBUG("message received: ~p", [Json]),
    ok = file:write(Log, [Msg, "\n"]),
    case Json of
        #{<<"op">> := 7} -> {stop, {shutdown, reconnect}, S};
        _ -> {keep_state, handle_ws_message(Json, S)}
    end;
connected(info, Msg, State) ->
    handle_common(Msg, State).

await_ack(info, {gun_ws, ConnPid, _StreamRef, {text, Msg}},
          S=#state{connection=#connection{pid=ConnPid}}) ->
    Json = decode_msg(Msg, S),
    case Json of
        #{<<"op">> := 11} ->
            {next_state, connected, handle_ws_message(Json, S)};
        _ -> {keep_state, S, [postpone]}
    end;
await_ack(cast, heartbeat, _State) ->
    ?LOG_INFO("received heartbeat while awaiting ack, disconnecting"),
    {stop, disconnected};
await_ack(info, Msg, State) ->
    handle_common(Msg, State).

%% helper functions

handle_common({gun_down, _, _, _, _}, _State) ->
    ?LOG_INFO("gun connection lost"),
    {stop, {shutdown, disconnected}};
handle_common({gun_ws, _, _, {close, _, _}}, _State) ->
    ?LOG_INFO("websocket closed"),
    {stop, {shutdown, disconnected}}.

decode_msg(Msg, #state{log=Log}) ->
    Json = jsone:decode(Msg),
    ?LOG_DEBUG("messge received: ~p", [Json]),
    ok = file:write(Log, [Msg, "\n"]),
    Json.

handle_ws_message(Msg=#{<<"op">> := Op, <<"d">> := Data}, S0) ->
    S1 = handle_ws_message_(Op, Msg, S0),
    Router = discordant_sup:get_router(),
    discord_router:route_raw(Router, Msg),
    update_session_id(Data, S1).

update_session_id(null, S0) -> S0;
update_session_id(Msg, S0) ->
    case maps:get(<<"session_id">>, Msg, undefined) of
        undefined -> S0;
        null -> S0;
        SessionId ->
            ?LOG_INFO("session id set to ~p", [SessionId]),
            S0#state{session_id=SessionId}
    end.

handle_ws_message_(0, #{<<"t">> := <<"MESSAGE_REACTION_ADD">>,
                        <<"d">> := Msg}, S0) ->
    Router = discordant_sup:get_router(),
    #{<<"user_id">> := UserId} = Msg,
    if UserId =:= S0#state.user_id -> ok;
       true ->
           discord_router:route_react(Router, Msg)
    end,
    S0;
handle_ws_message_(0, #{<<"d">> := Msg}, S0) ->
    % TODO compare session ids
    % TODO ensure we only use a single session ID
    S1 = if S0#state.user_id =:= undefined ->
                case maps:get(<<"user">>, Msg, undefined) of
                    undefined -> S0;
                    User ->
                        SX = S0#state{user_id=maps:get(<<"id">>, User)},
                        ?LOG_INFO("set user id to ~p", [SX#state.user_id]),
                        SX
                end;
            true -> S0
         end,
    handle_mentions(Msg, S1),
    S1;
handle_ws_message_(10, #{<<"d">> := #{<<"heartbeat_interval">> := IV}},
                   State) ->
    if State#state.heartbeat =:= undefined -> ok;
       true ->
           ?LOG_INFO("removing previous heartbeat"),
           discord_heartbeat:remove_heartbeat(State#state.heartbeat)
    end,
    ?LOG_INFO("installing heartbeat on interval ~p", [IV]),
    Ref = discord_heartbeat:create_heartbeat(IV, self()),
    State#state{heartbeat=Ref};
handle_ws_message_(11, _Msg, State) ->
    ?LOG_INFO("received heartbeat ack"),
    State.

send_message(#connection{pid=ConnPid, stream_ref=StreamRef}, OpCode, Msg) ->
    gun:ws_send(ConnPid, StreamRef,
                {text, jsone:encode(#{<<"op">> => OpCode, <<"d">> => Msg})}).

disconnect(#connection{pid=ConnPid, stream_ref=StreamRef, ref=MRef},
           Code, Msg) ->
    gun:ws_send(ConnPid, StreamRef, {close, Code, Msg}),
    demonitor(MRef),
    ok = gun:shutdown(ConnPid).

connect_(Next, State) ->
    ApiServer = discordant_sup:get_api_server(),
    BinGateway = discord_api:get_gateway(ApiServer),
    "wss://" ++ Gateway = binary:bin_to_list(BinGateway),
    ?LOG_INFO("connecting to discord gateway"),
    {ok, ConnPid} = gun:open(Gateway, 443, #{protocols => [http]}),
    {ok, _Protocol} = gun:await_up(ConnPid),
    MRef = monitor(process, ConnPid),
    gun:ws_upgrade(ConnPid, "/?v=6&encoding=json"),
    ?LOG_INFO("connected to discord with pid ~p", [ConnPid]),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
            {ok, Log} = file:open("events.log", [append, {encoding, unicode}]),
            Connection = #connection{pid=ConnPid,
                                     stream_ref=StreamRef,
                                     ref=MRef},
            {next_state, Next,
             State#state{url=Gateway,
                         connection=Connection,
                         log=Log}};
        {gun_response, ConnPid, _StreamRef, _Fin, _Status, _Headers} ->
            {stop, ws_upgrade_failed, State};
        {gun_error, ConnPid, _StreamRef, Reason} ->
            ?LOG_ERROR("gun error: ~p", [Reason]),
            {stop, ws_upgrade_failed, State}
    after 2000 -> {stop, timeout}
    end.

handle_mentions(#{<<"author">> := #{<<"id">> := UID}}, #state{user_id=UID}) ->
    ok;
handle_mentions(Msg=#{<<"mentions">> := Mentions}, #state{user_id=UID}) ->
    Me = lists:filter(fun(#{<<"id">> := ID}) -> ID =:= UID end, Mentions),
    if length(Me) > 0 ->
           Router = discordant_sup:get_router(),
           discord_router:route_msg(Router, Msg);
       true -> ok
    end;
handle_mentions(_Msg, _State) ->
    ok.
