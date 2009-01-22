-module(ajax_login).
-compile(export_all).
-include("session.hrl").
-include("authdb.hrl").

login(Struct, _Session, _Req) ->
    U = binary_to_list(obj:get_value(<<"uid">>, Struct)),
    P = binary_to_list(obj:get_value(<<"password">>, Struct)),
    case authdb:auth(U, P) of
        {ok, Realm} ->
	    SessionId = session:guid(),
	    H = mochiweb_cookies:cookie("enge2-sid", SessionId, [{path, "/"}]), 
	    io:format("cookie is ~p~n", [H]),

            session:new_session({auth, SessionId}, ?AUTHKEY_TIMEOUT, #authkey{user = U, realm = Realm}),
            {{obj, [{"event", <<"login-ok">>}, {"reply", <<"ok">>}]}, [H]};

	_ -> {{obj, [{"event", <<"login-fail">>}, {"error", <<"Bad login">>}]}, []}
    end.

logout(_Struct, Session, _Req) ->
    H = mochiweb_cookies:cookie("enge2-sid", "", [{path, "/"}]), 
    case Session of
        {error, _} -> void;
	_ -> session:end_session(Session#session.sid)
    end,
    H = mochiweb_cookies:cookie("enge2-sid", "", [{path, "/"}]), 
    {{obj, [{"event", <<"logout">>}, {"ok", []}]}, [H]}.

check(_Struct, Session, _Req) ->
    case Session of
        {error, _} -> 
            {{obj, [{"event", <<"not-loged-in">>}, {"reply", <<"ok">>}]}, []};
	_ ->
            {{obj, [{"event", <<"login-ok">>}, {"reply", <<"ok">>}]}, []}
    end.
