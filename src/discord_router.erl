-module(discord_router).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% public API
-export([start_link/0, set_routes/3, set_hooks/2, route_msg/2,
         route_react/2, route_raw/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {msg=#{} :: maps:map(atom(), mfa() | string() | [string()]),
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
    % TODO use cached values
    {ok, #state{}}.

handle_call({set_routes, Msg, React}, _From, State) ->
    discordant_config:set_value(msg_routes, Msg),
    discordant_config:set_value(react_routes, Msg),
    {reply, ok, State#state{msg=Msg, react=React}};
handle_call({set_hooks, Hooks}, _From, State) ->
    discordant_config:set_value(hooks, Hooks),
    {reply, ok, State#state{hooks=Hooks}}.

handle_cast({msg, Msg=#{<<"content">> := Content}}, State=#state{msg=Routes}) ->
    case binary:split(Content, <<" ">>, [global, trim_all]) of
        [_, <<"help">>|_] ->
            handle_response({reply, build_help(Routes), []}, Msg);
        [_, Cmd|Rest] ->
            ?LOG_INFO("looking up ~p", [Cmd]),
            case maps:get(Cmd, Routes, undefined) of
                undefined ->
                    handle_response({reply, <<"no such command">>, []}, Msg);
                #{call := {M, F, A}, args := Args} ->
                    if length(Rest) < length(Args) ->
                           Response = {reply, <<"not enough arguments">>, []},
                           handle_response(Response, Msg);
                       true ->
                           ApiPid = discordant_sup:get_api_server(),
                           Response = apply(M, F, A ++ [Rest, ApiPid, Msg]),
                           handle_response(Response, Msg)
                    end
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

build_help(Routes) ->
    lists:foldl(fun build_help/2, <<"help:">>, maps:to_list(Routes)).

build_help({Cmd, Map}, Acc) ->
    #{args := Args, help := Help} = Map,
    FullCmd = arg_join([Cmd|lists:map(fun list_to_binary/1, Args)], <<" ">>),
    BinHelp = list_to_binary(Help),
    Line = <<FullCmd/binary, " - ", BinHelp/binary>>,
    <<Acc/binary, "\n", Line/binary>>.

arg_join([H|T], Delim) ->
    lists:foldl(fun(X, Acc) ->
                        <<Acc/binary, Delim/binary, "{", X/binary, "}">>
                end, H, T).
