%% @author Dmitry Chernyak <losthost@narod.ru>
%% @copyright Copyleft 2009 

%% @doc ErlaNGine request/reply dispatcher.

% %% @type request() = {obj, [request_data()]}
% %% @type request_data() = module() + action() + event_spec() + args()
% %% @type module() = {module::binary(), word()}
% %% @type action() = {<<"action">>, word()}
% %% @type event_spec() = {<<"event">>, word()}
% %% @type args() = {<<"data">>, term()}

% %% @type word() = binary(). A binary string valid for erlang atom and for JavaScript string.

% %% @type reply() = {obj, [event(), reply()]}
% %% @type event() = {<<"event>>, iolist_to_binary(event_value())}
% %% @type event_value() = [event_spec(), "-", result_type()]. If event_spec was omited, module():action() is used.
% %% @type result() = {result_type(), result_value()}
% %% @type result_type() = ok | fail | atom(). Suffix for reply's event. Can be any value, but ok | fail are suggested
% %% @type result_value() = [] | binary() | list() | json()

-module(enge).
-compile(export_all).

-include("session.hrl").
-include("authkey.hrl").

compose_reply({Type, Result}, Event) ->
    {obj, [{"event", iolist_to_binary([Event, "-",  atom_to_list(Type)])},
           {"reply", Result}]};

% case when handler returns wrong format by mistake
compose_reply(R, E) ->
    io:format("compose reply error: ~p, ~p~n",[R, E]),
    {obj, [{"event", <<"enge2_error">>}, {"reply", <<"internal error">>}]}.

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
            try Module:Action(obj:get_value(<<"data">>, Struct), Session, Req) of
                {Result, Headers} ->
                    E = case obj:get_value(<<"event">>, Struct) of
                        undefined -> [M, <<":">>, A];
                        Event -> Event
                    end,
                    {reply, {compose_reply(Result, E), Headers}};

                _ -> {respond, {500, [], []}}     % Internal Server Error
            catch
                Error:Reason ->
                io:format("Happens ~p when calling ~p:~p with reason: ~p~n",[Error, Module, Action, Reason]),
                {respond, {501, [], []}}   % Wrong request XXX both no action found _and_ a runtime error
            end;

        _ ->
            Code = case Session of
                expired -> 403;         % Forbiden
                _ -> 404                % Not found
            end,
            {respond, {Code, [], []}}
    end.

