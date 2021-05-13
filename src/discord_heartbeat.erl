-module(discord_heartbeat).

-export([create_heartbeat/2, remove_heartbeat/1]).
-export_type([ref/0]).

-opaque ref() :: timer:tref().

-spec create_heartbeat(integer(), pid()) -> ref().
create_heartbeat(Interval, Pid) ->
    {ok, TRef} = timer:apply_interval(Interval, discord_gateway, heartbeat,
                                      [Pid]),
    discord_gateway:heartbeat(Pid),
    TRef.

-spec remove_heartbeat(ref()) -> ok.
remove_heartbeat(Ref) ->
    {ok, cancel} = timer:cancel(Ref),
    ok.
