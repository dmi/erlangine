-module(ajax_doc).
-compile(export_all).
-include("session.hrl").
-include("authdb.hrl").

% XXX restrict export
compose_attachment(Attachments) ->
    compose_attachment(Attachments, []).

compose_attachment([], Acc) ->
	 {"_attachments", {obj, Acc}};

compose_attachment([{Name, CType, Data} | T], Acc) ->
	Att = {Name, {obj, [{"content-type", list_to_binary(CType)},
						{"data", Data}]}},
	compose_attachment(T, [Att | Acc]).

save(Struct, Session, _Req) ->
    Keys = ["title", "anno", "text", "id", "rev"],
    [Title, Anno, Text, Id_old, Rev_old] = obj:get_values(Keys, Struct),
    #session{opaque = #authkey{user = U = Db}} = Session,
    {Y, M, D} = date(),

    AttrList = [{"type", <<"writing">>},
	        {"date", {obj, [{"Y", Y}, {"M", M}, {"D", D}]}},
	        {"author", list_to_binary(U)},
	        {"title", Title},
	        {"anno", Anno}],

    AttrList1 = case Rev_old of
        <<>> -> AttrList;
        Rev_old -> [{"_rev", Rev_old} | AttrList]
    end,

    AttrList2 = case Text of
        <<>> -> AttrList1;
        _ ->
            Att = compose_attachment([{"text", "text/html", Text}]),
            lists:reverse([Att | AttrList1])
    end,

    Obj = {obj, AttrList2},

    io:format("Save doc: ~p: ~p~nObj: ~p~n", [U, Title, Obj]),

    %{obj,[{"ok",true},
    %      {"id",<<"8d80981151b8cecd024b804c0c51c97b">>},
    %	   {"rev",<<"101943079">>}]}
    CreateResult = case Id_old of
        <<>> ->
            SaveNature = <<"saved">>,
            docs:create(Db, Obj);
        Id_old ->
            SaveNature = <<"updated">>,
            docs:create(Db, Id_old, Obj)
    end,

    case CreateResult of
		{ok, Id, Rev} -> 
			io:format("doc id=~p, rev=~p~n",[Id, Rev]),
			{{obj,
			  [{"event", <<"docsave-ok">>},
			   {"reply", SaveNature},
			   {"id", Id},
			   {"rev", Rev}]},
			  []};
		{error, Reason} ->
			io:format("failure ~p~n",[Reason]),
			{{obj,
			  [{"event", <<"docsave-fail">>},
			   {"error", <<"document not saved">>}]},
			 []}
	end.

save_att(Struct, Session, _Req) ->
    Keys = ["id", "rev", "name", "ctype", "data"],
    [Id, Rev, Name, CType, Data] = obj:get_values(Keys, Struct),
    #session{opaque = #authkey{user = Db}} = Session,
    Att = compose_attachment([{Name, CType, Data}]),
    Obj = [{"_id", Id},
	       {"_rev", Rev},
		   Att],
	case docs:create(Db, Obj) of
		{ok, Id, Rev} -> 
			io:format("doc id=~p, rev=~p~n",[Id, Rev]),
			{{obj,
			  [{"event", <<"attsave-ok">>},
			   {"reply", <<"ok">>},
			   {"id", Id},
			   {"rev", Rev}]},
			  []};
		{error, Reason} ->
			io:format("failure ~p~n",[Reason]),
			{{obj,
			  [{"event", <<"attsave-fail">>},
			   {"error", <<"attachment not saved">>}]},
			 []}
	end.

parse_view_response(R) ->
    parse_view_response(obj:get_value("rows",R), []).

parse_view_response([], Acc) ->
    lists:reverse(Acc);

parse_view_response([Doc | T], Acc) ->
    V = obj:get_value("value", Doc),
    parse_view_response(T, [V | Acc]).
    
search(_Struct, Session, _Req) ->
    #session{opaque = #authkey{user = Db}} = Session,
    case docs:doc_get(Db, "_view/writings/all") of
        {ok, Response} ->
	    Docs = parse_view_response(Response),
	    {{obj,
	      [{"event", <<"docsearch-done">>},
	       {"reply", Docs}]},
	      []};
	{error, Reason} ->
		io:format("failure ~p~n",[Reason]),
		{{obj,
		  [{"event", <<"docsearch-fail">>},
		   {"error", <<"search failed">>}]},
		 []}
    end.

doc_get(Struct, Session, _Req) ->
    Keys = ["id", "rev"],
    [Id, Rev] = obj:get_values(Keys, Struct),
    #session{opaque = #authkey{user = Db}} = Session,
    case docs:doc_get(Db, binary_to_list(Id) ++ "?rev=" ++ binary_to_list(Rev)) of
        {ok, Response} ->
            Doc = Response,
	    {{obj,
	      [{"event", <<"document-ok">>},
	       {"reply", Doc}]},
	      []};
	{error, Reason} ->
		io:format("failure ~p~n",[Reason]),
		{{obj,
		  [{"event", <<"document-fail">>},
		   {"error", <<"Document retrieve failed">>}]},
		 []}
    end.

attach_get(Struct, Session, _Req) ->
    Keys = ["id", "rev"],
    [Id, Rev] = obj:get_values(Keys, Struct),
    #session{opaque = #authkey{user = Db}} = Session,
    case docs:attach_get(Db, binary_to_list(Id) ++ "?rev=" ++ binary_to_list(Rev)) of
        {ok, Doc} ->
	    {{obj,
	      [{"event", <<"attach-ok">>},
	       {"reply", Doc}]},
	      []};
	{error, Reason} ->
		io:format("failure ~p~n",[Reason]),
		{{obj,
		  [{"event", <<"attach-fail">>},
		   {"error", <<"Attach retrieve failed">>}]},
		 []}
    end.
