%%%-------------------------------------------------------------------
%% @doc discordant public API
%% @end
%%%-------------------------------------------------------------------

-module(discordant_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Host = getenv_or_error("DISCORD_HOST"),
    Token = getenv_or_error("DISCORD_TOKEN"),
    discordant_sup:start_link(Host, Token).

stop(_State) ->
    ok.

%% internal functions

getenv_or_error(Env) ->
    case os:getenv(Env) of
        false -> throw({missing_envvar, Env});
        V -> V
    end.
