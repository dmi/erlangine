-module(destination).

-behaviour(gen_server).
-import(lists, [foreach/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0,
         new/5, update/6, remove/2, destination/2, destinations/1,
         reset/0]).

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
new(Uid, Parent, Title, Anno, Props) -> gen_server:call(?MODULE, {new, {Uid, Parent, Title, Anno, Props}}).

% check parent and title
% abort, noparent; abort, duptitle; ok
check_parent_and_title(Parent, Title) ->
    case mnesia:wread({destination, Parent}) of  % check parent and lock it
        [] -> mnesia:abort(noparent);
        [P] when is_record(P, destination) ->
            case mnesia:index_match_object(#destination{parent = Parent,
                                                        title = Title, _ = '_'},
                                           parent) % check title collision
            of
                [] -> ok;
                _ -> mnesia:abort(duptitle)
            end
    end.

update(Uid, Id, Parent, Title, Anno, Props) ->
    Row = #destination{id = {Uid, Id}, parent = {Uid, Parent}, uid=Uid, title = Title, anno = Anno, props = Props},
    F = fun() ->
        Old = mnesia:wread(Id),
        if Old#destination.parent /= Parent;
           Old#destination.title /= Title ->
               check_parent_and_title(Parent, Title)
        end,
        mnesia:write(Row)
    end,
    tr(F).

% check empty destination
remove(Uid, Id) ->
    I = {Uid, Id},
    Oid = {destination, I},
    F = fun() ->
        case mnesia:index_read(destination, I, parent) of
            [] -> ok;
            _ -> mnesia:abort(notempty)
        end,
        mnesia:delete(Oid)
    end,
    tr(F).

destination(Uid, Id) ->
    I = {Uid, Id},
    Oid = {destination, I},
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

handle_call({new, {Uid, Parent, Title, Anno, Props}}, _From, State) ->
    Id = session:guid(),
    Row = #destination{id = {Uid, Id}, parent = {Uid, Parent}, uid= Uid, title = Title, anno = Anno, props = Props},
    F = fun() ->
        check_parent_and_title(Parent, Title),
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
