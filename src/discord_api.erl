-module(discord_api).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0, get_gateway/1, send_message/3, send_message/4,
         send_message_reply/3, send_reaction/4, get_guild/2, get_roles/2,
         create_role/3, get_message/3, get_user/2, add_member_role/4,
         delete_role/3, get_guild_members/2, connect/2, remove_member_role/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(DISCORD_HOST, "discord.com").

-record(connection, {pid :: pid(),
                     ref :: reference()
                    }).
-record(state, {url :: string() | undefined,
                token :: string() | undefined,
                connection :: #connection{} | undefined
               }).

-type embed() :: maps:map(binary(), image()).
-type image() :: maps:map(binary(), binary()).

%% API functions

-spec start_link() -> any().
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec get_gateway(pid()) -> binary().
get_gateway(Pid) ->
    gen_server:call(Pid, get_gateway).

-spec send_message(pid(), binary(), binary()) -> ok.
send_message(Pid, ChannelId, Message) ->
    send_message(Pid, ChannelId, Message, []).

-spec send_message(pid(), binary(), binary(), [embed()]) -> ok.
send_message(Pid, ChannelId, Message, Embeds) ->
    gen_server:cast(Pid, {send_message, ChannelId, Message, Embeds}).

-spec send_message_reply(pid(), binary(), binary()) ->
    {ok, #{binary() => any()}}.
send_message_reply(Pid, ChannelId, Message) ->
    gen_server:call(Pid, {send_message, ChannelId, Message}).

-spec send_reaction(pid(), binary(), binary(), binary()) -> ok.
send_reaction(Pid, ChannelId, MessageId, Reaction) ->
    gen_server:cast(Pid, {send_reaction, ChannelId, MessageId, Reaction}).

-spec get_guild(pid(), binary()) -> #{binary() => any()}.
get_guild(Pid, GuildId) ->
    gen_server:call(Pid, {get_guild, GuildId}).

-spec get_roles(pid(), binary()) -> [#{binary() => any()}].
get_roles(Pid, GuildId) ->
    gen_server:call(Pid, {get_roles, GuildId}).

-spec create_role(pid(), binary(), binary()) -> ok.
create_role(Pid, GuildId, RoleName) ->
    gen_server:cast(Pid, {create_role, GuildId, RoleName}).

-spec get_message(pid(), binary(), binary()) -> #{binary() => any()}.
get_message(Pid, ChannelId, MessageId) ->
    gen_server:call(Pid, {get_message, ChannelId, MessageId}).

-spec get_user(pid(), binary()) -> #{binary() => any()}.
get_user(Pid, UserId) ->
    gen_server:call(Pid, {get_user, UserId}).

-spec add_member_role(pid(), binary(), binary(), binary()) -> ok.
add_member_role(Pid, GuildId, UserId, RoleId) ->
    gen_server:cast(Pid, {add_member_role, GuildId, UserId, RoleId}).

-spec remove_member_role(pid(), binary(), binary(), binary()) -> ok.
remove_member_role(Pid, GuildId, UserId, RoleId) ->
    gen_server:cast(Pid, {remove_member_role, GuildId, UserId, RoleId}).

-spec delete_role(pid(), binary(), binary()) -> ok.
delete_role(Pid, GuildId, RoleId) ->
    gen_server:cast(Pid, {delete_role, GuildId, RoleId}).

-spec connect(pid(), string()) -> ok.
connect(Pid, Token) ->
    gen_server:cast(Pid, {connect, Token}).

-spec get_guild_members(pid(), binary()) -> [#{binary() => any()}].
get_guild_members(Pid, GuildId) ->
    gen_server:call(Pid, {get_guild_members, GuildId}).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call(get_gateway, _From, State) ->
    #{<<"url">> := Url} = hget("/api/gateway/bot", State),
    {reply, Url, State};
handle_call({send_message, ChannelId, Message, Embeds}, _From, S0) ->
    ?LOG_INFO("sending message to ~p: ~p", [ChannelId, Message]),
    {S1, R} = send_message_(binary:bin_to_list(ChannelId), Message,
                            Embeds, S0),
    {reply, {ok, R}, S1};
handle_call({get_guild, GuildId}, _From, State) ->
    ?LOG_INFO("requesting info for guild ~s", [GuildId]),
    R = hget(<<"/api/guilds/", GuildId/binary>>, State),
    {reply, R, State};
handle_call({get_roles, GuildId}, _From, State) ->
    ?LOG_INFO("requesting roles for guild ~s", [GuildId]),
    R = hget(<<"/api/guilds/", GuildId/binary, "/roles">>, State),
    {reply, R, State};
handle_call({get_message, ChannelId, MessageId}, _From, State) ->
    ?LOG_INFO("requesting message ~s:~s", [ChannelId, MessageId]),
    R = hget(<<"/api/channels/", ChannelId/binary, "/messages/",
               MessageId/binary>>, State),
    {reply, R, State};
handle_call({get_user, UserId}, _From, State) ->
    ?LOG_INFO("requesting user ~s", [UserId]),
    R = hget(<<"/api/users/", UserId/binary>>, State),
    {reply, R, State};
handle_call({get_guild_members, GuildId}, _From, State) ->
    ?LOG_INFO("requesting guild member list for ~s", [GuildId]),
    R = hget(<<"/api/guilds/", GuildId/binary, "/members?limit=100">>, State),
    {reply, R, State}.

handle_cast({connect, Token}, S) ->
    {ok, ConnPid} = gun:open(?DISCORD_HOST, 443),
    {ok, _Protocol} = gun:await_up(ConnPid),
    MRef = monitor(process, ConnPid),
    Conn = #connection{pid=ConnPid, ref=MRef},
    {noreply, S#state{url=?DISCORD_HOST, token=Token, connection=Conn}};
handle_cast({send_message, ChannelId, Message}, S0) ->
    ?LOG_INFO("sending message to ~p: ~p", [ChannelId, Message]),
    {S1, _} = send_message_(binary:bin_to_list(ChannelId), Message, [], S0),
    {noreply, S1};
handle_cast({send_reaction, ChannelId, MessageId, Reaction}, S0) ->
    ?LOG_INFO("sending reaction to ~s#~s: ~s",
              [ChannelId, MessageId, Reaction]),
    {S1, _} = send_reaction_(ChannelId, MessageId, Reaction, S0),
    {noreply, S1};
handle_cast({create_role, GuildId, RoleName}, State) ->
    ?LOG_INFO("creating new role ~s#~s~n", [GuildId, RoleName]),
    post(<<"/api/guilds/", GuildId/binary, "/roles">>,
         #{<<"name">> => RoleName,
           <<"mentionable">> => true},
         State),
    {noreply, State};
handle_cast({add_member_role, GuildId, UserId, RoleId}, State) ->
    hput(<<"/api/guilds/", GuildId/binary, "/members/", UserId/binary,
           "/roles/", RoleId/binary>>, State),
    {noreply, State};
handle_cast({remove_member_role, GuildId, UserId, RoleId}, State) ->
    hdelete(<<"/api/guilds/", GuildId/binary, "/members/", UserId/binary,
              "/roles/", RoleId/binary>>, State),
    {noreply, State};
handle_cast({delete_role, GuildId, RoleId}, State) ->
    ?LOG_INFO("deleting role ~s#~s~n", [GuildId, RoleId]),
    hdelete(<<"/api/guilds/", GuildId/binary, "/roles/", RoleId/binary>>,
            State),
    {noreply, State}.

handle_info({gun_down, ConnPid, _, _, _},
            S=#state{connection=#connection{pid=ConnPid}}) ->
    ?LOG_INFO("gun lost api connection"),
    gun:await_up(ConnPid),
    ?LOG_INFO("gun regained api connection"),
    {noreply, S};
handle_info({gun_error, ConnPid, _, Reason},
            S=#state{connection=#connection{pid=ConnPid}}) ->
    ?LOG_ERROR("discord api error: ~p~n", [Reason]),
    {stop, error, S};
handle_info({'DOWN', MRef, process, ConnPid, Reason},
            S=#state{connection=#connection{pid=ConnPid, ref=MRef}}) ->
    ?LOG_ERROR("discord api disconnected ~p~n", [Reason]),
    {stop, disconnected, S}.

%% helper functions

hget(Url, #state{connection=Connection, token=Token}) ->
    StreamRef = gun:get(Connection#connection.pid, Url,
                        [{<<"authorization">>, "Bot " ++ Token}]),
    jsone:decode(read_body(Connection, StreamRef)).

read_body(#connection{pid=ConnPid}, StreamRef) ->
    % TODO handle non-200 status
    receive
        {gun_response, ConnPid, StreamRef, fin, _Status, _Headers} ->
            no_data;
        {gun_response, ConnPid, StreamRef, fin, 204, _Headers} ->
            no_data;
        {gun_response, ConnPid, StreamRef, nofin, 200, _Headers} ->
            receive_data(ConnPid, StreamRef, <<>>)
    after 1000 -> throw(timeout)
    end.

receive_data(ConnPid, StreamRef, Acc) ->
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            receive_data(ConnPid, StreamRef, <<Acc/binary, Data/binary>>);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            <<Acc/binary, Data/binary>>
    after 1000 -> throw(timeout)
    end.

post(Uri, Body, #state{connection=Connection, token=Token}) ->
    StreamRef = gun:post(Connection#connection.pid, Uri,
                         [{<<"authorization">>, "Bot " ++ Token},
                          {<<"content-type">>, <<"application/json">>}],
                         jsone:encode(Body)),
    jsone:decode(read_body(Connection, StreamRef)).

hput(Uri, #state{connection=Connection, token=Token}) ->
    StreamRef= gun:put(Connection#connection.pid, Uri,
                       [{<<"authorization">>, "Bot " ++ Token},
                        {<<"content-length">>, <<"0">>}]),
    case read_body(Connection, StreamRef) of
        no_data -> no_data;
        Data -> jsone:decode(Data)
    end.

hdelete(Uri, #state{connection=Connection, token=Token}) ->
    StreamRef= gun:delete(Connection#connection.pid, Uri,
                          [{<<"authorization">>, "Bot " ++ Token},
                           {<<"content-length">>, <<"0">>}]),
    case read_body(Connection, StreamRef) of
        no_data -> no_data;
        Data -> jsone:decode(Data)
    end.

send_message_(ChannelId, Message, Embeds, State) ->
    Body = post("/api/channels/" ++ ChannelId ++ "/messages",
                #{<<"content">> => Message,
                  <<"embeds">> => Embeds,
                  <<"allowed_metions">> => [<<"users">>, <<"roles">>]},
                State),
    {State, Body}.

send_reaction_(ChannelId, MessageId, Reaction, State) ->
    URI = uri_string:normalize(<<"/api/channels/", ChannelId/binary,
                                 "/messages/", MessageId/binary, "/reactions/",
                                 Reaction/binary, "/@me">>),
    {State, hput(URI, State)}.
