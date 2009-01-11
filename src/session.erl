%
% session is {sid, expires, opaque} unspecial storage with expiration support
%
-module(session).

-include_lib("stdlib/include/qlc.hrl").
-include("session.hrl").

-behaviour(gen_server).
-import(lists, [foreach/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0, add/3, get/1, get/2, remove/1, list_sessions/0, reset/0, test_engine/0]).

-compile(export_all).

%
% interface
%

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% XXX to handle stop call
stop() -> gen_server:call(?MODULE, stop).

% Expire is an expiration period in msecs
add(Sid, Expire, Opaque) -> gen_server:call(?MODULE, {add, Sid, Expire, Opaque}).

% returns expired | nosession | session()
get(Sid, Expire) -> gen_server:call(?MODULE, {get, Sid, Expire}).
get(Sid) -> gen_server:call(?MODULE, {get, Sid, 0}).

remove(Sid) -> gen_server:call(?MODULE, {remove, Sid}).

% cleanup all expired sessions
expire() -> gen_server:call(?MODULE, expire).

list_sessions() -> gen_server:call(?MODULE, list).

%
% run once
%

% ram-only fast table
reset() ->
    mnesia:delete_table(session),
    mnesia:create_table(session, [{attributes, record_info(fields, session)}]).

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
    {Code, Result} = mnesia:transaction(F),
    case Code of
        atomic -> Result;
	_ -> error
    end.

%
% gen_server
%

init([]) ->
    mnesia:create_table(session, [{attributes, record_info(fields, session)}]),
    ok = mnesia:wait_for_tables([session], 30000),
    io:format("Module: ~p ok~n",[?MODULE]),
    {ok, []}.

handle_call({add, Sid, Expire, Opaque}, _From, State) ->
    Row = #session{sid = Sid, expires = expire_time(Expire), opaque = Opaque},
    F = fun() ->
                mnesia:write(Row)
        end,
    Reply = tr(F),
    {reply, Reply, State};

% when Expire > 0, expiration will be reset
% when Expire < 0, session will be marked expired
% when Expire = 0, expiration leaves
% -> {error, nosession} | {error, expired} | #session()
handle_call({get, Sid, Expire}, _From, State) ->
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
    Reply = tr(F),
    {reply, Reply, State};

handle_call({remove, Sid}, _From, State) ->
    F = fun() ->
        mnesia:delete({session, Sid})
    end,
    Reply = tr(F),
    {reply, Reply, State};

handle_call(list, _From, State) ->
    Result = do(qlc:q([X || X <- mnesia:table(session)])),
    {reply, Result, State};

handle_call(expire, _From, State) ->
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
    Reply = tr(F),
    {reply, Reply, State};

handle_call(Other, _From, State) ->
    io:format("Unknown ~p request: ~p~n", [?MODULE, Other]),
    {reply, unknown, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

test_engine() ->
    {atomic, ok} = session:reset(),
    session:start(),
    ok = session:add("sid1", 1, {o1, o2}),
    ok = session:add("sid2", 10, {o3, o4}),
    nosession = session:get("sid0"),
    receive
    after 1000 -> ok
    end,
    expired = session:get("sid1"),
    #session{opaque = {o3, o4}}  = session:get("sid2"),
    io:format("list before expire: ~p~n",[session:list_sessions()]),
    1 = session:expire(),
    io:format("list after 1st expire: ~p~n",[session:list_sessions()]),
    io:format("timeout 3 secs to test update...~n",[]),
    receive
    after 3000 -> ok
    end,
    #session{opaque = {o3, o4}}  = session:get("sid2", 5),
    io:format("list after update: ~p~ntimeout 10 secs...~n",[session:list_sessions()]),
    receive
    after 10000 -> ok
    end,
    1 = session:expire(),
    io:format("list after 2nd expire: ~p~n",[session:list_sessions()]).

