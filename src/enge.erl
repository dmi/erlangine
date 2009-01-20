-module(enge).
-compile(export_all).

-include("session.hrl").
-include("authdb.hrl").

% {Result, Headers} | error
dispatch(Struct, Session, Req) ->

    M = obj:get_value(<<"module">>, Struct),
    Module = list_to_atom("ajax_" ++ binary_to_list(M)),

    Grant = case Session of
	{error, expired} ->
            realmdb:check_access(expired, Module);

        {error, nosession} ->
            realmdb:check_access(guest, Module);

        #session{opaque = #authkey{realm=Realm}} ->
	    io:format("Grant check: ~p ~p~n",[Realm, Module]),
            realmdb:check_access(Realm, Module)
    end,
    
    io:format("Grant: ~p~n",[Grant]),
    case Grant of
        true ->
            A = obj:get_value(<<"action">>, Struct),
            Action = list_to_atom(binary_to_list(A)),
	    % XXX case and introduce reply/respond type here
	    try Module:Action(Struct, Session, Req) of
                {Result, Headers} ->
                    {reply, {Result, Headers}};
		_ -> {respond, {500, [], []}}     % Internal Server Error
	    catch
	        Error:Reason ->
		    io:format("Happens ~p when calling ~p:~p with reason: ~p~n",[Error, Module, Action, Reason]),
		    {respond, {501, [], []}}   % Not Implemented XXX both no action found _and_ a runtime error
	    end;

        _ ->
            Code = case Session of
	        expired -> 403;             % Forbiden
		_ -> 404                    % Not found
	    end,
	    {respond, {Code, [], []}}

    end.

