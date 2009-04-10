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
                     [{name, <<"Оценка">>}, {type, score5}]]}]},
     []}.
