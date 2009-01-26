-module(ajax_test).
-compile(export_all).

-include("session.hrl").

test(Struct, Session, _Req) ->
    io:format("Test issued: ~p ~p~n",[Struct, Session]),
    case Session of
        #session{sid = {auth, Sid}} ->
            {{ok, list_to_binary(["Test ok the sid is ", Sid])}, []};
        _ ->
            {{ok, <<"No session">>}, []}
    end.
