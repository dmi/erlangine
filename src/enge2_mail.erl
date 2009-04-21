%% @author Dmitry Chernyak <losthost@narod.ru>
%% @copyright Copyleft 2009 

%% @doc E-Mail sender. Uses smtp_client.

-module(enge2_mail).

-compile(export_all).

send(To, Subject, Body) ->
    {ok, Pid} = smtp_fsm:start(),
    {ok, _Ehlo} = smtp_fsm:ehlo(Pid),
    From = case enge2_conf:option(mail_from) of
        undefined -> "erlangine@localhost";
        From1 -> From1
    end,
    Msg = email_msg:simp_msg(From, To, Subject, Body),
    io:format("msg: ~p~n",[Msg]),
    ok = smtp_fsm:sendemail(Pid, From, To, Msg),
    io:format("msg ckp~n",[]),
    ok = smtp_fsm:close(Pid).
