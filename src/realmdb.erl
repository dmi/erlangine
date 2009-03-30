-module(realmdb).

-behaviour(gen_server).
-import(lists, [foreach/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0,
         set_realm/3, remove_realm/1, check_access/2, realms/0,
	 reset/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(realms, {realm, descr, modules}).

%
% interface
%

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

set_realm(Realm, Descr, Modules) -> gen_server:call(?MODULE, {set_realm, {Realm, Descr, Modules}}).

remove_realm(Realm) -> gen_server:call(?MODULE, {remove_realm, Realm}).

check_access(Realm, Module) -> gen_server:call(?MODULE, {check_access, {Realm, Module}}).

realms() -> gen_server:call(?MODULE, realms).

%
% run once
%

initial_tables() ->
    [%% The realmdb table
     {realms, admin, "Administrator", []},
     {realms, user, "Regular User", [ajax_test, ajax_doc, ajax_dest]},
     {realms, guest, "Guest", [ajax_login, ajax_register, ajax_pwreco]},
     {realms, expired, "Expired Session", [ajax_login]}
     ].

reset() ->
    mnesia:delete_table(realms),
    mnesia:create_table(realms, [{attributes, record_info(fields, realms)},
								  {disc_copies, [node()]}]),
    F = fun() ->
			foreach(fun mnesia:write/1, initial_tables())
        end,
    tr(F).

%
% gen_server
%

init([]) ->
    A = mnesia:wait_for_tables([realms], 30000),
    io:format("Module ~p: ~p~n",[?MODULE, A]),
    A = ok,
    {ok, {}}.

handle_call({set_realm, {Realm, Descr, Modules}}, _From, State) ->
    Reply = case is_list(Modules) of
        true ->
	    Row = #realms{realm=Realm, descr=Descr, modules=Modules},
	    F = fun() -> mnesia:write(Row) end,
	    tr(F);
	_ -> {error, "Modules must be a list"}
    end,
    {reply, Reply, State};

handle_call({remove_realm, Realm}, _From, State) ->
    Reply = case Realm of
        admin -> {error, "Persistent record"};
	_ ->
	    Oid = {realms, Realm},
	    F = fun() ->
			mnesia:delete(Oid)
		end,
	    tr(F)
    end,
    {reply, Reply, State};

% check is Realm allowed to call Module
% returns bool()
handle_call({check_access, {Realm, Module}}, _From, State) ->
    Reply = case Realm of
        admin -> true;
	_ ->
	    M = do(qlc:q([X#realms.modules || X <- mnesia:table(realms),
					   (X#realms.realm =:= Realm
					    orelse X#realms.realm =:= guest)
				  ])),

	    case is_list(M) of
		true -> lists:member(Module, lists:flatten(M));
		_ -> false
	    end
    end,
    {reply, Reply, State};

handle_call(realms, _From, State) ->
    Result = do(qlc:q([X || X <- mnesia:table(realms)])),
    {reply, Result, State};

handle_call(Other, _From, State) ->
    io:format("Unknown ~p request: ~p~n", [?MODULE, Other]),
    {reply, unknown, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%
%  internals
%

do(Q) ->
    F = fun() ->
                qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

tr(F) ->
    {Code, Result} = mnesia:transaction(F),
    case Code of
        atomic -> ok;
	_ -> {error, Result}
    end.
