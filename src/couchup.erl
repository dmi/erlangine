%% @author Dmitry Chernyak <losthost@narod.ru>
%% @copyright Copyleft 2009 

%% @doc CouchDB document uploader
%%
%%      Supports !(inc:FILENAME) include directive.
%%
%%      Verifies documents for changes before upload, so it is good for view uploads.
%%
%%      uses eCouch fork from the ErlaNGinE project.
%%      uses strex module from the ErlaNGinE project.

-module(couchup).
-export([upload_dir/3, upload_doc/3]).

-include_lib("kernel/include/file.hrl").

%% @spec upload_dir(db(), dir(), suffix()) -> [{file(), result()}]
%% @doc Upload dir()/*.suffix() files into db()
%% @type db() = string(). Database name.
%% @type dir() = string(). Filesystem directory which contains files to upload.
%% @type result() = {ok, id(), rev()} | {error, reason()}
upload_dir(Db, Dir, Suffix) ->
    {ok, FList} = file:list_dir(Dir),
    Files = lists:filter(fun(X) -> lists:suffix(Suffix, X) end,
                         FList),
    upload_docs(Db, Dir, Files, []).

upload_docs(_, _, [], Acc) -> Acc;
upload_docs(Db, Dir, [Doc | T], Acc) ->
    R = upload_doc(Db, Dir, Doc),
    upload_docs(Db, Dir, T, [{Doc, R} | Acc]).

%% @spec upload_doc(db(), dir(), file()) -> result()
%% @doc Upload dir()/file() into db()
%%
%%      File can contain include directve !(inc:FILENAME).
%%
%%      Document Id is determined from "_id" property in the file.
%%
%%      File MUST NOT contain "_rev" property.
upload_doc(Db, Dir, DocFile) ->
    Expanded = expand_doc(Dir, DocFile),
    JSON = engejson:decode(list_to_binary(Expanded)),
    Id = engejson:get_value("_id", JSON),
    case ecouch:doc_get(Db, Id) of
        {ok, Doc} ->
            Rev = engejson:get_value("_rev", Doc),
            Doc0 = engejson:delete_value("_rev", Doc),
            if
                Doc0 =/= JSON ->
                    io:format("updating id: ~p rev: ~p~n", [Id, Rev]),
                    JSON1 = engejson:set_value("_rev", Rev, JSON),
                    Result = ecouch:doc_update(Db, Id, JSON1),
                    io:format("result: ~p~n", [Result]),
                    ecouch:decode_result(Result);
                true ->
                    io:format("no changes~n"),
                    {ok, Id, Rev}
            end;
        {fail, [{<<"error">>,<<"not_found">>},
                {<<"reason">>,<<"missing">>}]} ->
            io:format("creating new~n"),
            Result = ecouch:doc_create(Db, Id, JSON),
            ecouch:decode_result(Result);
        _ ->
            {error, broken_couchdb}
    end.

%% @spec expand_doc(dir(), docfile()) -> iolist()
%% @doc Substitute each !(inc: FILENAME) with file FILENAME
expand_doc(Dir, DocFile) ->
    {ok, Bin} = file:read_file(Dir ++ "/" ++ DocFile),
    Str = binary_to_list(Bin),
    parse_inc(Dir, Str, []).

parse_inc(Dir, Str, Acc) ->
    case strex:extract_meta("!(inc:", ")", Str) of
        {none, _, _} ->
            lists:reverse([Str | Acc]);
        {File, Before, After} ->
            {ok, Fbin} = file:read_file(Dir ++ "/" ++ File),
            parse_inc(Dir, After, [Fbin, Before | Acc])
    end.
