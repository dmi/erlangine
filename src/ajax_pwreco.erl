-module(ajax_pwreco).
-compile(export_all).
-include("session.hrl").
-include("authkey.hrl").
-include("authop.hrl").

% XXX no username check because no unicode here.
% whole method is ugly but quick. Welcome to improve ;-)
recover(Struct, _Session, _Req) ->
    [U, D, M, CL, CC] =
        engejson:get_values(["uid", "domain",
                             "email", "captchalink", "captchacode"],
                            Struct),

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
    Test = if
        SessionTest /= ok -> SessionTest;
        EMailTest /= ok -> EMailTest;
        true -> ok
    end,

    io:format("reco tests: ~p~n", [Test]),
    case Test of
        ok -> 
            case authdb:account({U, D}) of
                {account, Tokens, #authop{recovery = {email, M}}} ->
                    case lists:keysearch(passw, 1, Tokens) of
                        {value, {passw, Password}} ->
                            File = case enge2_conf:option(pwreco_file) of
                                undefined -> "conf/pwreco.txt";
                                File1 -> File1
                            end,
                            {ok, Template} = file:read_file(File),
                            case enge2_mail:send(M,
                                                 enge2_conf:option(pwreco_subj),
                                                 [Template, "\n", Password])
                            of
                                ok -> {{ok, []}, []};
                                {error, Reason} ->
                                    io:format("mail error: ~p~n", [Reason]),
                                    {{fail, "Error sending mail"}, []};
                                Any ->
                                    io:format("mail error: ~p~n", [Any]),
                                    {{fail, "Error 2 sending mail"}, []}
                            end;
                        false ->
                            {{fail, <<"No password token for account">>}, []}
                    end;

                {account, _, _} ->
                    {{fail, <<"E-mail mismatch for account">>}, []};

                {error, notfound} ->
                    {{fail, <<"Account not found">>}, []}
            end;

        Error ->
            {{fail, list_to_binary(Error)}, []}
    end.

prepare(_Struct, _Session, _Req) ->
    {CapUser, CapPw, CapWidth} = case enge2_conf:option(captcha) of
        undefined -> {"demo", "secret", 200};
        CapConf -> CapConf
    end,
    {Link, Code} = captchas:make_href(CapUser, CapPw, CapWidth), % possible to retrieve size from request
    session:new_session({captcha, Link}, ?CAPTCHA_TIMEOUT, Code),
    {{ok, [{"captcha", Link}, {"domains", enge2_conf:option(domains)}]}, []}.
