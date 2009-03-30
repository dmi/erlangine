-module(ajax_login).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").
-include("authop.hrl").

login(Struct, _Session, _Req) ->
    [U, D, P] = engejson:get_values(["uid", "domain", "password"], Struct),
    case authdb:auth({U, D}, {passw, P}) of
        {ok, #authop{realm = Realm}} ->
            SessionId = session:guid(),
            H = mochiweb_cookies:cookie("enge2-sid", SessionId, [{path, "/"}]), 
            io:format("cookie is ~p~n", [H]),

            session:new_session({auth, SessionId}, ?AUTHKEY_TIMEOUT, #authkey{user = {U, D}, realm = Realm}),
            {{ok, [[{"uid", U}, {"domain", D}]]}, [H]};

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
        #session{opaque = #authkey{user = {U, D}}} ->
            {{ok, [[{"uid", U}, {"domain", D}]]}, []}
    end.

domains(_Struct, _Session, _Req) ->
    {{ok, enge2_conf:option(domains)}, []}.
