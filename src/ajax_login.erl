-module(ajax_login).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").

login(Struct, _Session, _Req) ->
    U = binary_to_list(obj:get_value(<<"uid">>, Struct)),
    P = binary_to_list(obj:get_value(<<"password">>, Struct)),
    case authdb:auth(U, P) of
        {ok, Realm} ->
            SessionId = session:guid(),
            H = mochiweb_cookies:cookie("enge2-sid", SessionId, [{path, "/"}]), 
            io:format("cookie is ~p~n", [H]),

            session:new_session({auth, SessionId}, ?AUTHKEY_TIMEOUT, #authkey{user = U, realm = Realm}),
            {{ok, []}, [H]};

        _ -> {{fail, <<"Bad login">>}, []}
    end.

logout(_Struct, Session, _Req) ->
    H = mochiweb_cookies:cookie("enge2-sid", "", [{path, "/"}]), 
    case Session of
        {error, _} -> void;
        _ -> session:end_session(Session#session.sid)
    end,
    H = mochiweb_cookies:cookie("enge2-sid", "", [{path, "/"}]), 
    {{ok, []}, [H]}.

check(_Struct, Session, _Req) ->
    case Session of
        {error, _} -> 
            {{fail, <<"Not logged in">>}, []};
        #session{opaque = #authkey{user = U}} ->
            {{ok, list_to_binary(U)}, []}
    end.
