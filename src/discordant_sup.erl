-module(discordant_sup).
-behaviour(supervisor).

-export([start_link/0, get_api_server/0, get_gateway/0, get_router/0]).
-export([init/1]).

%% API functions

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_api_server() -> find_child(discord_api).

get_gateway() -> find_child(discord_gateway).

get_router() -> find_child(discord_router).

%% supervisor callbacks

init([]) ->
    SupFlags = #{strategy => one_for_rest,
                 intensity => 3,
                 period => 60},
    ChildSpecs = [#{id => discord_router,
                    start => {discord_router, start_link, []}},
                  #{id => discord_api,
                    start => {discord_api, start_link, []}},
                  #{id => discord_gateway,
                    start => {discord_gateway, start_link, []}}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% helper methods
find_child(Name) ->
    Children = supervisor:which_children(?MODULE),
    {_, Pid, _, _} = lists:keyfind(Name, 1, Children),
    Pid.
