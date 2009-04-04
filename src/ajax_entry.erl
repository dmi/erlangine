-module(ajax_entry).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").

entry_types(_Struct, _Session, _Req) ->
    {{ok, [{entry, [<<"Запись">>, <<"Кафе">>]},
           {fields, [string, text]}]},
     []}.

template(<<"Запись">>, _Session, _Req) ->
    {{ok, [{fields, [[{name, <<"Название">>},
                      {type, string}]]}]},
     []};
template(<<"Кафе">>, _Session, _Req) ->
    {{ok, [{fields, [[{name, <<"Название">>},
                      {type, string}],
                     [{name, test},
                      {type, text},
                      {value, blabla}]]}]},
     []};
template(Any, _Session, _Req) ->
    {{fail, Any}, []}.
