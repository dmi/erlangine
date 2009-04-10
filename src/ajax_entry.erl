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
                      {values, [[{name, <<"Название">>}, {value, <<"Портос">>}],
                                [{name, <<"Тип">>}, {value, <<"Кафе">>}]]}],
                     [{name, <<"Место">>},
                      {values,
                          [[{name, <<"Город">>}, {value, <<"Санкт-Петербург">>}],
                           [{name, <<"Адрес">>}, {value, <<"Кузнечный пер.">>}],
                           [{name, <<"Гео">>}]]}],
                     [{name, <<"Бизнес-ланч">>},
                      {values, [[{name, <<"Описание">>}, {value, <<"Стабильно приличная пища, среднего объема, не жирная. Можно длительно ходить">>}],
                                [{name, <<"Оценка">>}, {value, <<"Хороший">>}]]}],
                     [{name, <<"Коктейль">>},
                      {values,
                           [[{name, <<"Оценка">>}, {value, <<"Хороший">>}]]}]]}]},
     []};
template(Any, _Session, _Req) ->
    {{fail, Any}, []}.

value_names(_Struct, _Session, _Req) ->
    {{ok, [{values, [<<"Название">>, <<"Тип">>, <<"Город">>, <<"Адрес">>, <<"Гео">>, <<"Описание">>, <<"Оценка">>]}]},
     []}.
