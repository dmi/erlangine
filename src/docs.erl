-module(docs).
-compile(export_all).

newdb(Db) ->
	case ecouch:db_create(Db) of
	    {ok,{obj,[{<<"ok">>,true}]}} -> 
			io:format("db create ok: ~p~n",[Db]),
			ok;
		Err ->
			io:format("db create fail: ~p, ~p~n",[Db, Err]),
			error
	end.

% {ok, id(), rev()} | {error, reason()}
create(Db, Obj) ->
    %{obj,[{"ok",true},
    %      {"id",<<"8d80981151b8cecd024b804c0c51c97b">>},
    %	   {"rev",<<"101943079">>}]}
	case ecouch:doc_create(Db, Obj) of
		{ok, Result} -> 
			io:format("save doc: ~p~n", [Result]),
			case obj:get_values(["ok", "id", "rev"], Result) of
				[true, Id, Rev] ->
				    io:format("doc id=~p, rev=~p~n",[Id, Rev]),
				    {ok, Id, Rev};
				NoOk -> {error, {NoOk, Result}}
			end;
		Err -> {error, {unexpected, Err}}
	end.

create(Db, Name, Obj) ->
    %{obj,[{"ok",true},
    %      {"id",<<"8d80981151b8cecd024b804c0c51c97b">>},
    %	   {"rev",<<"101943079">>}]}
	case ecouch:doc_create(Db, Name, Obj) of
		{ok, Result} -> 
			io:format("save doc: ~p~n", [Result]),
			case obj:get_values(["ok", "id", "rev"], Result) of
				[true, Id, Rev] ->
				    io:format("doc id=~p, rev=~p~n",[Id, Rev]),
				    {ok, Id, Rev};
				NoOk -> {error, {NoOk, Result}}
			end;
		Err -> {error, {unexpected, Err}}
	end.

compose_mapreduce(Name, Map, Reduce) ->
	{Name, {obj, case Reduce of
					null ->  [{"map", list_to_binary(Map)}];
						_ -> [{"map", list_to_binary(Map)}, {"reduce", list_to_binary(Reduce)}]
					end}}.

compose_views(MRList) ->
	compose_views(MRList, []).

compose_views([], Acc) ->
	lists:reverse(Acc);
	
compose_views([{Name, Map, Reduce} | T], Acc) ->
	compose_views(T, [compose_mapreduce(Name, Map, Reduce) | Acc]).

% Db -> db(); map, reduce -> javascript()
% MRList -> {name(), map(), reduce())
create_view(Db, Name, MRList) ->
    ViewName = "_design/" ++ Name,
	Views = compose_views(MRList),
	Obj = {obj, [{"_id", list_to_binary(ViewName)},
	             {"language", <<"javascript">>},
				 {"views", {obj, Views}}]},
	io:format("create view: ~p~n", [Obj]),
	create(Db, ViewName, Obj).

reset_db(Db) ->
	ecouch:db_delete(Db),
	ecouch:db_create(Db),
	create_view("dmi","writings",[{"all", "function(doc){ if(doc.type == 'writing') emit(null, doc) }", null}]).
