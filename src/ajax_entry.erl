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
    {{ok, [{fields, [[{name, <<"Описание">>},
                      {values, [[{type, <<"Название">>}, {value, <<"Портос">>}],
                                [{type, <<"Тип">>}, {value, <<"Кафе">>}]]}],
                     [{name, <<"Место">>},
                      {values,
                          [[{type, <<"Город">>}, {value, <<"Санкт-Петербург">>}],
                           [{type, <<"Адрес">>}, {value, <<"Кузнечный пер.">>}],
                           [{type, <<"Гео">>}]]}],
                     [{name, <<"Бизнес-ланч">>},
                      {values, [[{type, <<"Описание">>}, {value, <<"Стабильно приличная пища, среднего объема, не жирная. Можно длительно ходить">>}],
                                [{type, <<"Оценка">>}, {value, <<"Хороший">>}]]}],
                     [{name, <<"Коктейль">>},
                      {values,
                           [[{type, <<"Оценка">>}, {value, <<"Хороший">>}]]}]]}]},
     []};
template(Any, _Session, _Req) ->
    {{fail, Any}, []}.

value_types(_Struct, _Session, _Req) ->
    {{ok, [string, text]},
     []}.
