-module(discordant).

-export([connect/1, connect/2, set_routes/2]).

-spec connect(string()) -> ok.
connect(Token) ->
    connect(Token, []).

-spec connect(string(), [Options]) -> ok
    when Options :: monitor.
connect(Token, Options) ->
    ApiPid = discordant_sup:get_api_server(),
    discord_api:connect(ApiPid, Token),
    GatewayPid = discordant_sup:get_gateway(),
    discord_gateway:connect(GatewayPid, Token),
    case lists:member(monitor, Options) of
        true ->
            monitor(process, discordant_sup),
            ok;
        false -> ok
    end.

set_routes(Msg, React) ->
    Pid = discordant_sup:get_router(),
    discord_router:set_routes(Pid, Msg, React).
