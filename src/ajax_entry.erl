-module(ajax_entry).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").

entry_templates(_Struct, _Session, _Req) ->
    {{ok, [{entry, [<<"Запись">>, <<"Кафе">>]}]},
     []}.

template(<<"Запись">>, _Session, _Req) ->
    {{ok, [{fields, [[{name, <<"Название">>}]]}]},
     []};
template(<<"Кафе">>, _Session, _Req) ->
    {{ok, [{fields, [[{name, <<"Название">>},
                      {values, [[{name, <<"Название">>}, {type, text}, {value, <<"Портос">>}]]}],
                     [{name, <<"Тип">>},
                      {values, [[{name, <<"Тип">>}, {type, text}, {value, <<"Кафе">>}]]}],
                     [{name, <<"Место">>},
                      {values,
                          [[{name, <<"Город">>}, {type, text}, {value, <<"Санкт-Петербург">>}],
                           [{name, <<"Адрес">>}, {type, text}, {value, <<"Кузнечный пер.">>}],
                           [{name, <<"Гео">>}]]}],
                     [{name, <<"Бизнес-ланч">>},
                      {values, [[{name, <<"Описание">>}, {type, text}, {value, <<"<p>Стабильно приличная пища, среднего объема, не жирная.</p><p>Можно длительно ходить</p>">>}],
                                [{name, <<"Оценка">>}, {type, score5}, {value, <<"4">>}]]}],
                     [{name, <<"Коктейль">>},
                      {values,
                           [[{name, <<"Оценка">>}, {type, score5}, {value, <<"3">>}]]}]]}]},
     []};
template(Any, _Session, _Req) ->
    {{fail, Any}, []}.

value_names(_Struct, _Session, _Req) ->
    {{ok, [{values, [[{name, <<"Название">>}, {type, text}],
                     [{name, <<"Тип">>}, {type, text}],
                     [{name, <<"Город">>}, {type, text}],
                     [{name, <<"Адрес">>}, {type, text}],
                     [{name, <<"Гео">>}, {type, text}],
                     [{name, <<"Описание">>}, {type, text}],
                     [{name, <<"Цена">>}, {type, string}],
                     [{name, <<"Оценка">>}, {type, score5}]]}]},
     []}.

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

read(Struct, Session, _Req) ->
    [Id, Rev] = engejson:get_values(["id", "rev"], Struct),
    #session{opaque = #authkey{user = Uid}} = Session,
    Reply = entry:read(Uid, Id),
    {Reply, []}.

search(Struct, Session, _Req) ->
    #session{opaque = #authkey{user = Uid}} = Session,
    Reply = entry:read(Uid, "_design/store/_view/entries"),
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
