-module(destination).

-behaviour(gen_server).
-import(lists, [foreach/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0,
         new/5, update/6, remove/2, destinations/1,
         reset/0]).

-include_lib("stdlib/include/qlc.hrl").

-include("destination.hrl").

%
% interface
%

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) -> ok.

stop() -> gen_server:call(?MODULE, stop) -> ok.

% return id()
new(Uid, Parent, Title, Anno, Props) -> gen_server:call(?MODULE, {new, {Uid, Parent, Title, Anno, Props}}).

update(Uid, Id, Parent, Title, Anno, Props) -> ok.

remove(Uid, Id) -> ok.

destinations(Uid) -> ok.

%
% run once
%

reset() ->
    mnesia:delete_table(destination),
    mnesia:create_table(destination, [{attributes, record_info(fields, destination)},
                                      {disc_copies, [node()]}]).

%
% gen_server
%

init([]) ->
    A = mnesia:wait_for_tables([destination], 30000),
    io:format("Module ~p: ~p~n",[?MODULE, A]),
    A = ok,
    {ok, {}}.

handle_call({new, {Uid, Id, Title, Anno, Props}}, _From, State) ->
    Row = #destination{id = {Uid, Id}, title = Title, anno = Anno, props = Props},
    F = fun() ->
                case mnesia:wread({destination, Uid}) of
                    [] -> mnesia:write(Row);
                    [A] when is_record(A, authdb) -> exists;
                    Other -> % XXX is it needed?
                        io:format("authdb error:  ~p~n", [Other]),
                        {error, Other}
                end
        end,
    Reply = tr(F),
    {reply, Reply, State};

handle_call({auth, {Uid, Passw}}, _From, State) ->
    U = do(qlc:q([X#authdb.realm || X <- mnesia:table(authdb),
                                   X#authdb.uid =:= Uid,
                                   X#authdb.passw =:= Passw
                          ])),
    Reply = case U of
                [Realm] -> {ok, Realm};
                [] -> {error, notfound}
            end,
    {reply, Reply, State};

handle_call({remove, {Uid, Passw}}, _From, State) ->
    Oid = {authdb, Uid},
    F = fun() ->
                [A] = mnesia:read(Oid),
                Passw = A#authdb.passw,
                mnesia:delete(Oid)
        end,
    Reply = tr(F),
    {reply, Reply, State};

handle_call(accounts, _From, State) ->
    Result = do(qlc:q([{X#authdb.uid, X#authdb.name} || X <- mnesia:table(authdb)])),
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
    mnesia:transaction(F).
