-module(discord_router).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% public API
-export([start_link/0, set_routes/3, set_hooks/2, route_msg/2,
         route_react/2, route_raw/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {msg=#{} :: maps:map(binary(), mfa()),
                react=[] :: [mfa()],
                hooks=#{} :: maps:map(binary(), mfa())
               }).

%% public api

start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_routes(Pid, Msg, React) ->
    gen_server:call(Pid, {set_routes, Msg, React}).

set_hooks(Pid, Hooks) ->
    gen_server:call(Pid, {set_hooks, Hooks}).

route_msg(Pid, Msg) ->
    gen_server:cast(Pid, {msg, Msg}).

route_react(Pid, Msg) ->
    gen_server:cast(Pid, {react, Msg}).

route_raw(Pid, Msg) ->
    gen_server:cast(Pid, {raw, Msg}).

%% gen_server callbacks

init([]) ->
    ?LOG_ERROR("starting the router"),
    {ok, #state{}}.

% TODO cache these values
handle_call({set_routes, Msg, React}, _From, State) ->
    {reply, ok, State#state{msg=Msg, react=React}};
handle_call({set_hooks, Hooks}, _From, State) ->
    {reply, ok, State#state{hooks=Hooks}}.

handle_cast({msg, Msg=#{<<"content">> := Content}}, State=#state{msg=Routes}) ->
    case binary:split(Content, <<" ">>, [global, trim_all]) of
        [_, Cmd|Rest] ->
            ?LOG_INFO("looking up ~p", [Cmd]),
            case maps:get(Cmd, Routes, undefined) of
                undefined -> ok;
                #{call := {M, F, A}} ->
                    ApiPid = discordant_sup:get_api_server(),
                    handle_response(apply(M, F, A ++ [Rest, ApiPid, Msg]), Msg)
            end
    end,
    {noreply, State};
handle_cast({react, React}, State=#state{react=Routes}) ->
    lists:foreach(fun({M, F, A}) -> M:F(A ++ [React]) end, Routes),
    {noreply, State};
handle_cast({raw, Msg}, State=#state{hooks=Routes}) ->
    case maps:get(<<"t">>, Msg) of
        null -> ok;
        Cmd ->
            case maps:get(Cmd, Routes, undefined) of
                undefined -> ok;
                {M, F, A} ->
                    ApiPid = discordant_sup:get_api_server(),
                    apply(M, F, A ++ [ApiPid, Msg])
            end
    end,
    {noreply, State}.

%% internal functions

handle_response({reply, Reply, _Args}, Msg) ->
    ApiPid = discordant_sup:get_api_server(),
    #{<<"channel_id">> := ChannelId} = Msg,
    discord_api:send_message(ApiPid, ChannelId, Reply);
handle_response(noreply, _Msg) -> ok.
