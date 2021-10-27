-module(discordant_config).
-behaviour(gen_server).

-export([start_link/0, set_value/2, get_value/1, has_value/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_value(Key, Value) ->
    gen_server:cast(get_pid(), {set, Key, Value}).

get_value(Key) ->
    gen_server:call(get_pid(), {get, Key}).

has_value(Key) ->
    gen_server:call(get_pid(), {has, Key}).

init([]) ->
    {ok, []}.

handle_call({get, Key}, _From, State) ->
    R = case maps:is_key(Key, State) of
            true -> {ok, maps:get(Key, State)};
            false -> not_found
        end,
    {reply, R, State};
handle_call({has, Key}, _From, State) ->
    {reply, maps:is_key(Key, State), State}.

handle_cast({set, Key, Value}, State) ->
    {noreply, State#{Key => Value}}.

get_pid() ->
    discordant_sup:get_config().
