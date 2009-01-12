-module(ajax_register).
-compile(export_all).
-include("session.hrl").
-include("authdb.hrl").

% XXX no username check because no unicode here.
% whole method is ugly but quick. Welcome to improve ;-)
account(Struct, _Session, _Req) ->
    Keys = ["name", "uid", "password", "repeat",
	    "email", "captchalink", "captchacode"],
    [N, U, P, P2, M, CL, CC] =
        lists:map(fun(A) -> binary_to_list(A) end, obj:get_values(Keys, Struct)),

    LL = length(P),
    SessionTest = case session:get({captcha, CL}, -1) of
        {error, expired} -> "Verification code expired";
        {error, nosession} -> "Bad verification code";
	#session{opaque = CS} when CC /= CS -> "Wrong verification code";
	#session{} -> ok;
	_ -> "Unknown error"
    end,
    EMailTest = case regexp:match(M, "^[a-z_0-9.]+@[a-z_0-9.]+\.[a-z][a-z][a-z]?$") of
        nomatch -> "Email is incorrect";
	{error, ReasonM} ->
            io:format("email match error: ~p~n",[ReasonM]),
	    "Internal error 1";
	{match, _MStart, _MLength} -> ok
    end,
    UidTest = case regexp:match(M, "^[a-zA-Z0-9_.@ ]+$") of
        nomatch -> "Email is incorrect";
	{error, ReasonU} ->
            io:format("email match error: ~p~n",[ReasonU]),
	    "Internal error 1";
	{match, _UStart, _ULength} -> ok
    end,
    Test = if
        SessionTest /= ok -> SessionTest;
	EMailTest /= ok -> EMailTest;
	UidTest /= ok -> UidTest;
        LL < 4 -> "Pasword is too short";
        P /= P2 -> "Paswords not match";
	EMailTest /= ok -> EMailTest;
        true -> ok
    end,

    io:format("reg tests: ~p~n", [Test]),
    case Test of
    	ok -> 
	    case authdb:new(U, P, user, N, M) of
		{atomic, ok} ->
		    case docs:reset_db(U) of
				ok -> 
					{{obj, [{"event", <<"register-ok">>}, {"reply", <<"ok">>}]}, []};
				error ->
					{{obj, [{"event", <<"register-fail">>}, {"error", <<"Db create failed">>}]}, []}
			end;

		{atomic, exists} ->
		    {{obj, [{"event", <<"register-fail">>}, {"error", <<"Account already exists">>}]}, []};

		Reason ->
		    io:format("authdb error when new: ~p~n",[Reason]),
		    {{obj, [{"event", <<"register-fail">>}, {"error", <<"Internal error 2">>}]}, []}
	    end;
	Error ->
		{{obj, [{"event", <<"register-fail">>}, {"error", list_to_binary(Error)}]}, []}
    end.

captcha(_Struct, _Session, _Req) ->
    {Link, Code} = captchas:make_href("demo", "secret", 200), % possible to retrieve size from request
    session:add({captcha, Link}, ?CAPTCHA_TIMEOUT, Code),
    {{obj, [{"event", <<"captcha">>}, {"link", list_to_binary(Link)}]},
     []}.
