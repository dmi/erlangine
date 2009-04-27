-module(ajax_entry).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").

entry_templates(_Struct, Session, _Req) ->
    #session{opaque = #authkey{user = Uid}} = Session,
    {ok, Templates} = entry:read(Uid, "_design/store/_view/template"),
    T = engejson:get_value(<<"rows">>, Templates),
    {{ok, T}, []}.

template(Id, Session, _Req) ->
    #session{opaque = #authkey{user = Uid}} = Session,
    Reply = entry:read(Uid, Id),
    {Reply, []}.

read(Struct, Session, _Req) ->
    [Id, Rev] = engejson:get_values(["id", "rev"], Struct),
    #session{opaque = #authkey{user = Uid}} = Session,
    Reply = entry:read(Uid, Id),
    {Reply, []}.

value_names(Struct, Session, _Req) ->
    [Field, _Type] = engejson:get_values(["field", "type"], Struct),
    FieldS = binary_to_list(Field),
    #session{opaque = #authkey{user = Uid}} = Session,
    {ok, Vals} = entry:read(Uid, "_design/store/_view/value"),
    V = lists:map(fun(X) -> engejson:get_value(<<"value">>, X) end,
                  engejson:get_value(<<"rows">>, Vals)),
    {ok, ValTop} = entry:read(Uid, "_design/store/_view/value_weight?group=true&startkey=[\"" ++ FieldS ++ "\"]&endkey=[\"" ++ FieldS ++ "\",\"\\u9999\"]"),
    T = lists:map(fun(X) ->
                      [[_Field, ValName, ValType], Weight] = engejson:get_values(["key", "value"], X),
                      {Weight, ValName, ValType}
                  end,
                  engejson:get_value(<<"rows">>, ValTop)),
    TS = lists:map(fun({_Weight, ValName, ValType}) -> [{name, ValName}, {type, ValType}] end,
                   lists:reverse(lists:keysort(1, T))),
    {{ok, [{top, TS}, {all, V}]}, []}.

save(Struct, Session, _Req) ->
    [Id, Rev, Destination, Fields] =
        engejson:get_values(["id", "rev", "destination", "fields"], Struct),
    #session{opaque = #authkey{user = Uid}} = Session,
    case if
        Id =:= <<>> ->
            entry:create(Uid, Destination, Fields);
        true ->
            entry:update(Uid, Id, Rev, Destination, Fields)
    end of
        {ok, Id1, Rev1} -> {{ok, [{id, Id1}, {rev, Rev1}]}, []};
        {error, _Reason} -> {{fail, <<"Save entry failed">>}, []}
    end.

search(Struct, Session, _Req) ->
    io:format("search: ~p", [Struct]),
    [Field, Value] = engejson:get_values(["field", "value"], Struct),
    #session{opaque = #authkey{user = Uid}} = Session,
    io:format("search: ~p ~p", [Field, Value]),
    Reply = case Field of
        undefined -> entry:read(Uid, "_design/store/_view/entries");
        _ ->
            FieldS = binary_to_list(Field),
            case Value of
                undefined -> entry:read(Uid, "_design/store/_view/search_field?startkey=[\"" ++ FieldS ++ "\"]&endkey=[\"" ++ FieldS ++ "\",\"\\u9999\"]");
                _ ->
                    ValueS = binary_to_list(Value),
                    entry:read(Uid, "_design/store/_view/search_field?key=[\"" ++ FieldS ++ "\", \"" ++ ValueS ++ "\"]")
        end
    end,
    {Reply, []}.

bulk_delete(_Uid, []) -> ok;

bulk_delete(Uid, [H | T]) ->
    [Id, Rev] = engejson:get_values(["id", "rev"], H),
    io:format("bulk_delete: ~p ~p~n",[Id, Rev]),
    entry:delete(Uid, Id, Rev),
    bulk_delete(Uid, T).

bulk_delete(Struct, Session, _Req) ->
    #session{opaque = #authkey{user = Uid}} = Session,
    ToDelete = Struct,
    bulk_delete(Uid, ToDelete),
    {{ok, []}, []}.
