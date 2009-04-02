-module(ajax_entry).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").

entry_types(_Struct, _Session, _Req) ->
    {{ok, [<<"Запись">>, <<"Кафе">>]}, []}.

type_fields(<<"Запись">>, _Session, _Req) ->
    {{ok, [{mandatory, [[{name, <<"Название">>},
                         {type, string}]]},
           {optional, [[{name, <<"Описание">>},
                        {type, text}]]},
           {types, []}]},
     []};
type_fields(<<"Кафе">>, _Session, _Req) ->
    {{ok, [{mandatory, [[{name, <<"Название">>},
                         {type, string}],
                        [{name, test},
                         {type, text}]]},
           {optional, [[{name, <<"Описание">>},
                        {type, text}]]},
           {types, []}]},
     []};
type_fields(Any, _Session, _Req) ->
    {{fail, Any}, []}.
