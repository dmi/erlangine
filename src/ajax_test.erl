-module(ajax_test).
-compile(export_all).

-include("session.hrl").

test(Struct, Session, _Req) ->
    io:format("Test issued: ~p ~p~n",[Struct, Session]),
    if is_record(Session, session) ->
        {auth, Sid} = Session#session.sid,
        {{obj, [{<<"event">>, <<"test">>}, {<<"reply">>, list_to_binary(["Test ok the sid is ", Sid])}]}, []};
       true ->
        {{obj, [{<<"event">>, <<"test">>}, {<<"reply">>, <<"No session">>}]}, []}
    end.
