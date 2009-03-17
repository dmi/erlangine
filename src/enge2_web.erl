%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for enge2.

-module(enge2_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

-include("authkey.hrl").

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),

    Sid = Req:get_cookie_value("enge2-sid"),
    Session = session:get_session({auth, Sid}, ?AUTHKEY_TIMEOUT),

    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
	        
		"enge" ->
		    %io:format("Req: ~p~n",[Req]),
		    %RB = Req:recv_body(),
		    %io:format("Body: ~p~n",[RB]),
		    Data = Req:parse_post(),
		    %io:format("Data: ~p~n",[Data]),
		    Json = proplists:get_value("json", Data),
		    Struct = engejson:decode(Json),

                    io:format("Enge request: ~p~n", [Struct]),

                    Reply = enge:dispatch(Struct, Session, Req),

                    io:format("Enge reply: ~p~n",[Reply]),

		    case Reply of

			{reply, {Result, Headers}} ->
			    DataOut = engejson:encode(Result),
			    Req:ok({"application/json", Headers, [DataOut]});

			{respond, Respond } -> Req:respond(Respond);

			_ -> Req:respond({404, [], []})

		    end;

                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
