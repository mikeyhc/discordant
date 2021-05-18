-module(discordant).

-export([connect/1, set_routes/2]).

-spec connect(string()) -> ok.
connect(Token) ->
    ApiPid = discord_sup:get_api_server(),
    discord_api:connect(ApiPid, Token),
    GatewayPid = discord_sup:get_gateway(),
    discord_gateway:connect(GatewayPid, Token).

set_routes(Msg, React) ->
    Pid = discord_sup:get_router(),
    discord_router:set_routes(Pid, Msg, React).
