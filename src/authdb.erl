%% @author Dmitry Chernyak <losthost@narod.ru>
%% @copyright Copyleft 2009 

%% @doc Accounts database with authentication
%%
%% Uses Mnesia storage.

%% @type authdb() = {authdb, uid(), tokens(), opaque()}. Account database record.
%% @type uid() = term(). User Id of any form.
%% @type tokens() = [token()].
%% @type token() = {tokentype(), authdata()}.
%% @type tokentype() = passw. Password authentication token. Can be improved.
%% @type opaque() = term(). Supplement data structure, returned from authdb:auth/2.

-module(authdb).

-behavior(gen_server).
-import(lists, [foreach/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/0, stop/0,
         new/3, update/3, remove/1, auth/2, accounts/0,
         reset/0, upgrade/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(authdb, {uid, tokens, opaque}).

% this only for upgrade in ErlaNGinE
-include_lib("authop.hrl").

%
% interface
%

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

%% @spec new(uid(), tokens(), opaque()) -> ok | {error, exists}
%% @doc Create new account
new(Uid, Tokens, Opaque) ->
    Row = #authdb{uid=Uid, tokens=Tokens, opaque=Opaque},
    F = fun() ->
        case mnesia:read({authdb,Uid}) of
            [] -> mnesia:write(Row);
            [A] when is_record(A, authdb) -> {error, exists}
        end
    end,
    tr(F).

%% @spec update(uid(), tokens(), opaque()) -> ok | {error, nonexists} | {error, baddtokens}
%% @doc Update account. This function doesn't alter uid().
update(Uid, Tokens, Opaque) when is_list(Tokens) ->
    Row = #authdb{uid=Uid, tokens=Tokens, opaque=Opaque},
    F = fun() ->
        case mnesia:read({authdb, Uid}) of
            [] -> {error, nonexists};
            [A] when is_record(A, authdb) -> mnesia:write(Row)
        end
    end,
    tr(F);

update(_Uid, _Tokens, _Opaque) -> {error, badtokens}.

%% @spec remove(uid()) -> {atomic, ok}
%% @doc Remove account.
remove(Uid) ->
    Oid = {authdb, Uid},
    F = fun() ->
        mnesia:delete(Oid)
    end,
    tr(F).

%% @spec auth(uid(), token()) -> {ok, opaque()} | {error, notfound}
%% @doc Authenticate to account.
auth(_Uid, {passw, Passw}) when Passw =:= <<>> ->
    {error, notfound};

auth(Uid, Token = {passw, _Passw}) ->
    F = fun() ->
        case mnesia:read({authdb, Uid}) of
            [] -> {error, notfound};
            [#authdb{tokens = Tokens, opaque = Opaque}] ->
                case lists:member(Token, Tokens) of
                    true -> {ok, Opaque};
                    _ -> {error, notfound}
                end
        end
    end,
    tr(F).

%% @spec accounts() -> [{uid(), opaque()}]
%% @doc List all accounts
accounts() ->
    do(qlc:q([{X#authdb.uid, X#authdb.opaque} || X <- mnesia:table(authdb)])).


%
% run once
%

reset() ->
    mnesia:delete_table(authdb),
    mnesia:create_table(authdb, [{attributes, record_info(fields, authdb)},
                                 {disc_copies, [node()]}]).

upgrade() ->
    mnesia:delete_table(authdb_conv),
    mnesia:create_table(authdb_conv, [{attributes, record_info(fields, authdb)},
                                      {disc_copies, [node()]}]),

    Fc = fun() ->
        mnesia:foldl(
            fun({authdb, Uid, Passw, Realm, Name, EMail}, []) ->
                Row = #authdb{uid = {list_to_binary(Uid), <<"localhost">>},
                        tokens = [{passw, list_to_binary(Passw)}],
                        opaque = #authop{realm = Realm,
                                         name = Name,
                                         recovery = {email, EMail},
                                         roles = []}},
                io:format("tr: ~p~n", [Row]),
                Res = mnesia:write(authdb_conv, Row, write),
                io:format("tr res: ~p~n", [Res])
            end,
            [],
            authdb)
    end,
    ResC = mnesia:transaction(Fc),
    io:format("convert table: ~p~n", [ResC]),
        
    %reset(),

    Fw = fun() ->
        mnesia:foldl(
            fun(Row, []) ->
                mnesia:write(authdb, Row, write)
            end,
            [],
            authdb)
    end.
    %tr(Fw).
    
%
% gen_server
%

init([]) ->
    A = mnesia:wait_for_tables([authdb], 30000),
    io:format("Module ~p: ~p~n",[?MODULE, A]),
    A = ok,
    {ok, {}}.

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
%  internals
%

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

tr(F) ->
    {atomic, Result} = mnesia:transaction(F),
    Result.
