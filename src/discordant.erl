-module(discordant).

-export([connect/1, connect/2, set_routes/2, set_hooks/1, user_id/0]).

-spec connect(string()) -> ok.
connect(Token) ->
    connect(Token, []).

-spec connect(string(), [Options]) -> ok
    when Options :: monitor.
connect(Token, Options) ->
    % discordant_config:set_value(discord_token, Token),
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

set_hooks(Hooks) ->
    Pid = discordant_sup:get_router(),
    discord_router:set_hooks(Pid, Hooks).

user_id() ->
    Pid = discordant_sup:get_gateway(),
    discord_gateway:user_id(Pid).
