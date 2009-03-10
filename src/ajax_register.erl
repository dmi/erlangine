-module(ajax_register).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").
-include("authop.hrl").

% XXX no username check because no unicode here.
% whole method is ugly but quick. Welcome to improve ;-)
account(Struct, _Session, _Req) ->
    [N, U, D, P, P2, M, CL, CC] =
        obj:get_values(["name", "uid", "domain", "password", "repeat",
                        "email", "captchalink", "captchacode"],
                       Struct),

    LL = length(binary_to_list(P)),
    SessionTest = case session:get_session({captcha, CL}, -1) of
        {error, expired} -> "Verification code expired";
        {error, nosession} -> "Bad verification code";
        #session{opaque = CS} when CC /= CS -> "Wrong verification code";
        #session{} -> ok;
        _ -> "Unknown error"
    end,
    EMailTest = case regexp:match(binary_to_list(M), "^[a-z_0-9.]+@[a-z_0-9.]+\.[a-z][a-z][a-z]?$") of
        nomatch -> "Email is incorrect";
        {error, ReasonM} ->
            io:format("email match error: ~p~n",[ReasonM]),
            "Internal error 1";
        {match, _MStart, _MLength} -> ok
    end,
    UidTest = case regexp:match(binary_to_list(U), "^[a-zA-Z][a-zA-Z0-9_.@ ]+$") of
        nomatch -> "Uid is incorrect";
        {error, ReasonU} ->
            io:format("uid match error: ~p~n",[ReasonU]),
            "Internal error 2";
        {match, _UStart, _ULength} -> ok
    end,
    DomainTest = case lists:member(D, [<<"localhost">>]) of
        true -> ok;
        false -> "Domain not allowed"
    end,
    Test = if
        SessionTest /= ok -> SessionTest;
        EMailTest /= ok -> EMailTest;
        UidTest /= ok -> UidTest;
        DomainTest /= ok -> DomainTest;
        LL < 4 -> "Pasword is too short";
        P /= P2 -> "Paswords not match";
        EMailTest /= ok -> EMailTest;
        true -> ok
    end,

    io:format("reg tests: ~p~n", [Test]),
    case Test of
        ok -> 
            case authdb:new({U, D}, [{passw, P}], #authop{realm = user, name = N, recovery = {email, M}}) of
                ok ->
                    case docs:reset_db({U, D}) of
                        ok -> 
                            {{ok, []}, []};
                        error ->
                            {{fail, <<"Db create failed">>}, []}
                    end;

                {error, exists} ->
                    {{fail, <<"Account already exists">>}, []};

                Reason ->
                    io:format("authdb error when new: ~p~n",[Reason]),
                    {{fail, <<"Internal error 2">>}, []}
            end;

        Error ->
            {{fail, list_to_binary(Error)}, []}
    end.

prepare(_Struct, _Session, _Req) ->
    {Link, Code} = captchas:make_href("demo", "secret", 200), % possible to retrieve size from request
    session:new_session({captcha, Link}, ?CAPTCHA_TIMEOUT, Code),
    {{ok, {obj, [{"captcha", Link}, {"domains", [<<"localhost">>]}]}}, []}.
