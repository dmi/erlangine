%% @author Dmitry Chernyak <losthost@narod.ru>
%% @copyright Copyleft 2009 

%% @doc Session server, unspecial storage with expiration support.
%%      Saves {SID, Opaque} data
%%      Uses Mnesia in-memory storage

%% @type session() = {session, sid(), expires(), opaque()}
%% @type sid() = term(). Session Id
%% @type opaque() = term(). Session data
%% @type expires() = term(). Expiration date/dime as in erlang:now()
%% @type expire() = integer(). Expiration period in msecs

-module(session).

-include_lib("stdlib/include/qlc.hrl").
-include("session.hrl").

-export([init/0, add/3, get_session/1, get_session/2, remove/1, list_sessions/0, expire/0, test_engine/0]).

%% @spec init() -> ok
%% @doc Create in-memory system table after start.
%%
%% Currently - one-node configuration only, but can be improved
init() ->
    % ram-only fast table
    {atomic, ok} = mnesia:create_table(session, [{attributes, record_info(fields, session)}]),
    ok.


%% @spec add(sid(), expire(), opaque()) -> ok
%% @doc Create new session, unconditionally replace old with the same key.
add(Sid, Expire, Opaque) ->
    Row = #session{sid = Sid, expires = expire_time(Expire), opaque = Opaque},
    F = fun() ->
                mnesia:write(Row)
        end,
    tr(F).


%% @spec get_session(sid()) -> {error, nosession} | {error, expired} | session()
%% @doc Return session(), not touch expiration time.
get_session(Sid) -> get_session(Sid, 0).


%% @spec get_session(sid(), expire()) -> {error, nosession} | {error, expired} | session()
%% @doc Return session(), change expiration time.
%%
%% <ul>
%% <li>When expire() &gt; 0, expiration will be set to expire() period;</li>
%% <li>When expire() &lt; 0, session will be marked expired immediately;</li>
%% <li>When expire() = 0, expiration leaves unchanged.</li>
%% </ul>
get_session(Sid, Expire) ->
    F = fun() ->
                case mnesia:read({session, Sid}) of
                    [S] -> 
            case expired(S#session.expires) of
                true -> {error, expired};
                _ ->
                    if Expire > 0 ->
                    Row = S#session{expires = expire_time(Expire)},
                    mnesia:write(Row);
                   Expire < 0 ->
                    Row = S#session{expires = now()},
                    mnesia:write(Row);
                   true -> ok
                end,
                S
            end;
            _ -> {error, nosession}
        end
        end,
    tr(F).


%% @spec remove(sid()) -> ok
%% @doc Remove session from database.
remove(Sid) ->
    F = fun() ->
        mnesia:delete({session, Sid})
    end,
    tr(F).


%% @spec expire() -> integer()
%% @doc Cleanup all expired sessions, returns the count.
%%
%% Must be called explicitly (gen_event supposed)
expire() ->
    F = fun() ->
        mnesia:foldl(fun(#session{sid = Sid, expires = Expires}, Counter) ->
        case expired(Expires) of
            true -> mnesia:delete({session, Sid}),
        Counter + 1;
        _ -> Counter
        end end,
        0,
        session)
    end,
    tr(F).


%% @spec list_sessions() -> [session()]
%% @doc List all database include expired sessions.
list_sessions() ->
    do(qlc:q([X || X <- mnesia:table(session)])).

%% @spec test_engine() -> ok
%% @doc Interactive, destructive automatic test.
%%
%% Note: it will destroy and recreate session tabe.
test_engine() ->
    {atomic, ok} = mnesia:delete_table(session),
    {atomic, ok} = mnesia:create_table(session, [{attributes, record_info(fields, session)}]),
    ok = mnesia:wait_for_tables([session],1000),
    ok = session:add("sid1", 1, {o1, o2}),
    ok = session:add("sid2", 10, {o3, o4}),
    {error, nosession} = get_session("sid0"),
    receive
    after 1000 -> ok
    end,
    {error, expired} = get_session("sid1"),
    #session{opaque = {o3, o4}}  = get_session("sid2"),
    io:format("list before expire: ~p~n",[session:list_sessions()]),
    1 = session:expire(),
    io:format("list after 1st expire: ~p~n",[session:list_sessions()]),
    io:format("timeout 3 secs to test update...~n",[]),
    receive
    after 3000 -> ok
    end,
    #session{opaque = {o3, o4}}  = get_session("sid2", 5),
    io:format("list after update: ~p~ntimeout 10 secs...~n",[session:list_sessions()]),
    receive
    after 10000 -> ok
    end,
    1 = session:expire(),
    io:format("list after 2nd expire: ~p~n",[session:list_sessions()]),
    ok.


%
% internals
%


% calculate expire time in period from now
expire_time(Period) ->
    {SM, SS, Sm} = now(),
    SS1 = SS + Period,
    if SS1 > 1000000 -> {SM + 1, SS1 - 1000000, Sm};
        true -> {SM, SS1, Sm}
    end.


expired(Expires) ->
    ExpPeriod = timer:now_diff(Expires, now()),
    ExpPeriod < 0.


do(Q) ->
    F = fun() ->
                qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.


% return Result or 'error'
% the error reason is not returned. U may log it right herein
tr(F) ->
    {atomic, Value} = mnesia:transaction(F),
    Value.
