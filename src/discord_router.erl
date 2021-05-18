-module(discord_router).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

%% public API
-export([start_link/0, set_routes/3, route_msg/2, route_react/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%% public api

start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_routes(Pid, Msg, React) ->
    gen_server:call(Pid, {set_routes, #{msg => Msg, react => React}}).

route_msg(Pid, Msg) ->
    gen_server:cast(Pid, {msg, Msg}).

route_react(Pid, Msg) ->
    gen_server:cast(Pid, {react, Msg}).

%% gen_server callbacks

init([]) ->
    {ok, #{msg => #{}, react => []}}.

handle_call({set_routes, Routes}, _From, _State) ->
    {noreply, Routes}.

handle_cast({msg, Msg=#{<<"content">> := Content}}, State=#{msg := Msg}) ->
    case binary:split(Content, <<" ">>, [global, trim_all]) of
        [_, Cmd|Rest] ->
            ?LOG_INFO("looking up ~p", [Cmd]),
            case maps:get(Cmd, Msg, undefined) of
                undefined -> ok;
                {M, F, A} -> M:F(A ++ [Rest, Msg])
            end
    end,
    {noreply, State};
handle_cast({react, React}, State=#{react := Routes}) ->
    lists:foreach(fun({M, F, A}) -> M:F(A ++ [React]) end, Routes),
    {noreply, State}.
