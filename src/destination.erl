-module(destination).

-behaviour(gen_server).
-import(lists, [foreach/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0,
         new/5, update/6, remove/2, destination/2, destinations/1,
         reset/0, test_engine/0]).

-include_lib("stdlib/include/qlc.hrl").

-include("destination.hrl").

%
% interface
%

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

%
% run once
%

reset() ->
    mnesia:delete_table(destination),
    mnesia:create_table(destination, [{attributes, record_info(fields, destination)},
                                      {disc_copies, [node()]}]),
    mnesia:add_table_index(destination, parent),
    mnesia:add_table_index(destination, uid).

% return id()
new(Uid, ParentId, Title, Anno, Props) -> gen_server:call(?MODULE, {new, {Uid, ParentId, Title, Anno, Props}}).

% check parent and title
% abort, noparent; abort, duptitle; ok
check_parent_and_title(Uid, ParentId, Title) ->
    Parent = {Uid, ParentId},
    if ParentId =/= 0 ->
        case mnesia:wread({destination, Parent}) of  % check parent and lock it
            [] -> mnesia:abort(noparent);
            [P] when is_record(P, destination) -> ok
        end;
       true -> ok
    end,
    case mnesia:index_match_object(#destination{parent = Parent,
                                                title = Title, _ = '_'},
                                   parent) % check title collision
    of
        [] -> ok;
        _ -> mnesia:abort(duptitle)
    end.

update(Uid, Id, ParentId, Title, Anno, Props) ->
    RowId = {Uid, Id},
    Row = #destination{id = RowId, parent = Parent = {Uid, ParentId}, uid=Uid, title = Title, anno = Anno, props = Props},
    F = fun() ->
        case mnesia:wread({destination, RowId}) of
            [] -> mnesia:abort(norecord);
            [Old] when Old#destination.parent =/= Parent;
                       Old#destination.title =/= Title ->
               check_parent_and_title(Uid, ParentId, Title)
        end,
        mnesia:write(Row)
    end,
    tr(F).

% check empty destination
remove(Uid, Id) ->
    RowId = {Uid, Id},
    Oid = {destination, RowId},
    F = fun() ->
        case mnesia:index_read(destination, RowId, parent) of
            [] -> ok;
            _ -> mnesia:abort(notempty)
        end,
        mnesia:delete(Oid)
    end,
    tr(F).

destination(Uid, Id) ->
    RowId = {Uid, Id},
    Oid = {destination, RowId},
    F = fun() ->
        mnesia:read(Oid)
    end,
    tr(F).

destinations(Uid) ->
    F = fun() ->
        mnesia:index_read(destination, Uid, uid)
    end,
    tr(F).

%
% gen_server
%

init([]) ->
    A = mnesia:wait_for_tables([destination], 30000),
    io:format("Module ~p: ~p~n",[?MODULE, A]),
    A = ok,
    {ok, {}}.

handle_call({new, {Uid, ParentId, Title, Anno, Props}}, _From, State) ->
    Id = session:guid(),
    Row = #destination{id = {Uid, Id}, parent = {Uid, ParentId}, uid= Uid, title = Title, anno = Anno, props = Props},
    F = fun() ->
        check_parent_and_title(Uid, ParentId, Title),
        mnesia:write(Row)
    end,
    Reply = case tr(F) of
        {atomic, ok} -> Id;
        Err -> Err
    end,
    {reply, Reply, State};

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

tr(F) ->
    mnesia:transaction(F).

%
%  tests
%

test_engine_data() ->
    [{"uid1", 0, "dest-1", "from uid1"},
     {"uid1", 0, "dest-2", "from uid1"},
     {"uid2", 0, "dest-1", "from uid2"},
     {"uid2", 0, "dest-2", "from uid2"}].

test_engine() ->
    mnesia:start(),
    crypto:start(),
    io:format("reset db~n"),
    destination:reset(),
    io:format("start server~n"),
    destination:start(),
    io:format("load test data:~n"),
    _Ids = [Id1, Id2 | _T] = lists:map(fun({Uid, ParentId, Title, Anno}) ->
                                    io:format("new level 1 destination: "),
                                    Id = destination:new(Uid, ParentId, Title, Anno, {}),
                                    io:format("~p~n",[Id]),
                                    Id
                                end,
                                test_engine_data()),
    io:format("new level 2 destination: "),
    Id1_1 = destination:new("uid1", Id1, "dest1-1", "from uid1 level 2", {}),
    io:format("~p~n",[Id1]),
    io:format("remove non-empty destination 1: "),
    {aborted, notempty} = destination:remove("uid1", Id1),
    io:format("ok~n"),
    io:format("update nonexisting destination: "),
    {aborted, norecord} = destination:update("uid1", "123", 0, "test", "test", {}),
    io:format("ok~n"),
    io:format("update existing destination: "),
    {atomic, ok} = destination:update("uid1", Id1_1, Id1, "dest1-1 updated", "updated from uid1 level2", {}),
    Upd1_0 = {destination, {"uid1", Id1_1}, {"uid1", Id1}, "uid1", "dest1-1 updated", "updated from uid1 level2", {}},
    io:format("check result against: ~p~n", [Upd1_0]),
    {atomic, [Upd1]} = destination:destination("uid1", Id1_1),
    io:format("update result: ~p~n", [Upd1]),
    Upd1_0 = Upd1,
    io:format("check: ok~n"),
    io:format("move existing destination: "),
    {atomic, ok} = destination:update("uid1", Id1_1, Id2, "dest2-1 moved", "moved from uid1 level2", {}),
    Upd2_0 = Upd1_0#destination{parent = {"uid1", Id2}, title = "dest2-1 moved", anno = "moved from uid1 level2"},
    io:format("check result against: ~p~n", [Upd2_0]),
    {atomic, [Upd2]} = destination:destination("uid1", Id1_1),
    io:format("update result: ~p~n", [Upd2]),
    Upd2_0 = Upd2,
    io:format("ok~n"),
    io:format("remove empty destination: "),
    {atomic, ok} = destination:remove("uid1", Id1),
    io:format("ok~n"),
    io:format("all destinations:~n"),
    All1 = destination:destinations("uid1"),
    All2 = destination:destinations("uid2"),
    io:format("uid1:~n~p~nuid2:~n~p~n",[All1, All2]),
    io:format("all complete~n").
