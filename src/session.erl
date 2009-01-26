%% @author Dmitry Chernyak <losthost@narod.ru>
%% @copyright Copyleft 2009 

%% @doc Session server, unspecial storage with expiration support.
%%
%% Saves {sid(), opaque()} data.
%%
%% Uses ETS storage.

%% @type session() = {session, sid(), expires(), opaque()}
%% @type sid() = term(). Session Id
%% @type opaque() = term(). Session data
%% @type expires() = term(). Expiration date/dime as in erlang:now()
%% @type expire() = integer(). Expiration period in msecs

-module(session).

-include("session.hrl").

-behaviour(gen_server).

%% gen_server callbacks
-export([start/0, stop/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([new_session/3, get_session/1, get_session/2, replace_session/3, end_session/1, list_sessions/0, expire/0, test_engine/0, guid/0]).

%
% Interface
%

%% @spec new_session(sid(), expire(), opaque()) -> ok | {error, exists}
%% @doc Create new session, unconditionally replace old with the same key.
new_session(Sid, Expire, Opaque) ->
    gen_server:call(?MODULE, {new_session, Sid, Expire, Opaque}).

%% @spec get_session(sid()) -> {error, nosession} | {error, expired} | session()
%% @doc Return session(), don't touch expiration time.
%%
%% Actually calls get_session(sid(), 0)
get_session(Sid) ->
    get_session(Sid, 0).

%% @spec get_session(sid(), expire()) -> {error, nosession} | {error, expired} | session()
%% @doc Return session(), change expiration time.
%%
%% <ul>
%% <li>When expire() &gt; 0, expiration will be set to expire() period;</li>
%% <li>When expire() &lt; 0, session will be marked expired immediately;</li>
%% <li>When expire() = 0, expiration leaves unchanged.</li>
%% </ul>
get_session(Sid, Expire) ->
    case ets:lookup(?MODULE, Sid) of
    [S] -> 
        case expired(S#session.expires) of
        true ->
            ets:delete(?MODULE, Sid),
            {error, expired};
        _ ->
            if
            Expire > 0 ->
                Row = S#session{expires = expire_time(Expire)},
                ets:insert(?MODULE, Row);
            Expire < 0 ->
                ets:delete(?MODULE, Sid);
            true -> ok
            end,
            S
        end;
    _ -> {error, nosession}
     end.

%% @spec replace_session(sid(), expire(), opaque()) -> ok | {error, nosession} | {error, expired}
replace_session(Sid, Expire, Opaque) ->
    Expired = case ets:lookup(?MODULE, Sid) of
        [#session{expires = Expires}] -> expired(Expires);
    _ -> nosession
    end, 
    case Expired of
        false ->
        Row = #session{sid = Sid, expires = expire_time(Expire), opaque = Opaque},
        ets:insert(?MODULE, Row),
        ok;
    nosession -> {error, nosession};
    true ->
        ets:delete(?MODULE, Sid),
        {error,  expired}
    end.


%% @spec end_session(sid()) -> ok
%% @doc Remove session from database.
end_session(Sid) ->
    ets:delete(?MODULE, Sid),
    ok.


%% @spec expire() -> integer()
%% @doc Cleanup all expired sessions, returns the count.
%%
%% Must be called explicitly (gen_event supposed)
expire() ->
    ets:safe_fixtable(?MODULE, true),
    Ret = expire(ets:first(?MODULE), 0),
    ets:safe_fixtable(?MODULE, false),
    Ret.
    
expire('$end_of_table', Counter) ->
    Counter;

expire(Sid, Counter) ->
    [#session{expires = Expires}] = ets:lookup(?MODULE, Sid),
    Counter1 = case expired(Expires) of
    true ->
        ets:delete(?MODULE, Sid),
        Counter + 1;
    _ -> Counter
    end,
    expire(ets:next(?MODULE, Sid), Counter1).

%% @spec list_sessions() -> [session()]
%% @doc List all database include expired sessions.
list_sessions() ->
    ets:tab2list(?MODULE).

%% @spec test_engine() -> ok
%% @doc Interactive, destructive automatic test.
%%
%% Note: it will destroy and recreate session tabe.
test_engine() ->
    io:format("start server~n"),
    session:start(),

    io:format("fill session table~n"),
    ok = session:new_session("sid1", 1, {o1, o2}),
    ok = session:new_session("sid2", 10, {o3, o4}),
    ok = session:new_session("sid3", 1, {o5, o6}),

    io:format("test nosession~n"),
    {error, nosession} = session:get_session("sid0"),

    io:format("test expiration~n"),
    receive after 1000 -> ok end,
    {error, expired} = session:get_session("sid1"),

    io:format("test noexpiration~n"),
    #session{opaque = {o3, o4}}  = session:get_session("sid2"),

    io:format("test expire()~n"),
    L1 = session:list_sessions(),
    2 = length(L1),
    io:format("list before expire: ~p~n",[L1]),
    1 = session:expire(),
    io:format("list after 1st expire: ~p~n", [session:list_sessions()]),

    io:format("test expiration update~n"),
    io:format("before update: ~p~n",[session:get_session("sid2", 5)]),
    io:format("after update: ~p~n",[session:get_session("sid2")]),

    io:format("test replace~n"),
    ok = session:replace_session("sid2", 5, {o7, o8}),
    #session{opaque = {o7, o8}} = session:get_session("sid2"),

    io:format("test replace expired~n"),
    io:format("timeout 5 secs...~n"),
    receive after 5000 -> ok end,
    {error, expired} = session:replace_session("sid2", 5, {o7, o8}),

    io:format("test replace expired~n"),
    {error, nosession} = session:replace_session("sid2", 5, {o7, o8}),

    [] = session:list_sessions(),
    io:format("all passed ok~n"),
    ok.


%% @spec guid() -> string()
%% @doc Generate unique Id
guid() ->
    <<I:160/integer>> = crypto:sha(term_to_binary({node(), make_ref(), now()})),
    erlang:integer_to_list(I, 16).

%
% gen_server interface
%

%% @spec start() -> {ok, pid()} | ignore | {error, Error}
%% @doc Start session server
%%
%% Error = {already_started, pid()} | term()
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> void
%% @doc Stop server
stop() -> gen_server:call(?MODULE, stop).


%
% gen_server internals
%

%% @spec init([]) -> ok
%% @doc Create in-memory ssession table after start. Called by start/1
init([]) ->
    %mnesia:delete_table(session), % XXX for migration period only
    ets:new(?MODULE, [set, named_table, public, {keypos, 2}]),
    io:format("Module: ~p ok~n",[?MODULE]),
    {ok, []}.


handle_call({new_session, Sid, Expire, Opaque}, _From, State) ->
    Expired = case ets:lookup(?MODULE, Sid) of
        [#session{expires = Expires}] -> expired(Expires);
    _ -> true
    end, 
    case Expired of
        true ->
        Row = #session{sid = Sid, expires = expire_time(Expire), opaque = Opaque},
        ets:insert(?MODULE, Row),
        {reply, ok, State};
    _ -> {reply, {error, exists}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, stopped, State};

handle_call(Other, _From, State) ->
    io:format("Unknown ~p request: ~p~n", [?MODULE, Other]),
    {reply, unknown, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

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
