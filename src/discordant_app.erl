%%%-------------------------------------------------------------------
%% @doc discordant public API
%% @end
%%%-------------------------------------------------------------------

-module(discordant_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    discordant_sup:start_link().

stop(_State) ->
    ok.
