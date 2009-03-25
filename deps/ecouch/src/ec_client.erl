%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%

%%%-------------------------------------------------------------------
%%% @private
%%% File:      ec_client.erl
%%% @author    Vitor Rodrigues <> []
%%% @copyright 2008 Vitor Rodrigues
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2008-04-02 by Vitor Rodrigues
%%%-------------------------------------------------------------------
-module(ec_client).
-author('Vitor Rodrigues').

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         
 %%--------------------------------------------------------------------
 %% macro definitions
 %%--------------------------------------------------------------------
 -define(SERVER, ?MODULE).
 -define(BINARY, {body_format, binary}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast({Operation, Host, Port, From}, State) ->
    Reply = case Operation of
        {get, Path, Options} ->
            QueryString = query_string(Options),
            Url = lists:flatten(io_lib:format("http://~s:~s~s~s", [Host, Port, Path, QueryString])),
            http_g_request(Url);
        {bin, Path, Options} ->
            QueryString = query_string(Options),
            Url = lists:flatten(io_lib:format("http://~s:~s~s~s", [Host, Port, Path, QueryString])),
            http_b_request(Url);
        {post, Path, Doc} ->
            Url = lists:flatten(io_lib:format("http://~s:~s~s", [Host, Port, Path])),
            http_p_request(post, Url, Doc);
        {post, Path, Doc, ContentType, Options} ->
            QueryString = query_string(Options),
            Url = lists:flatten(io_lib:format("http://~s:~s~s~s", [Host, Port, Path, QueryString])),
            http_p_request(post, Url, Doc, ContentType);
        {put, Path, Doc} ->
            Url = lists:flatten(io_lib:format("http://~s:~s~s", [Host, Port, Path])),
            http_p_request(put, Url, Doc);
        {put, Path, Doc, ContentType, Options} ->
            QueryString = query_string(Options),
            Url = lists:flatten(io_lib:format("http://~s:~s~s~s", [Host, Port, Path, QueryString])),
            http_p_request(put, Url, Doc, ContentType);
        {delete, Path, Options} ->
            QueryString = query_string(Options),
            Url = lists:flatten(io_lib:format("http://~s:~s~s~s", [Host, Port, Path, QueryString])),
            http_d_request(Url);
        _Other ->
            {error, "Bad operation"}
    end,
    gen_server:reply(From, Reply),
    {stop, "Normal", State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @spec handle_reply(reply(), check()) -> {ok, json()} | {fail, json()} | {error, reason()}
%% @doc Internal function for handling json http:request.
%% @type check() = string(). Status code to to distinguish between ok and fail states.
handle_reply(Reply, Check) ->
    case Reply of
        {error, Reason} ->
            {error, Reason};
        {ok, {{_, Status, _}, _Headers, Body}} ->
            try engejson:decode(Body) of
                Json ->
                    case Status of
                        Check -> {ok, Json};
                        _ -> {fail, Json}
                    end
            catch
                _Error:Reason ->
                    {error, Reason}
            end
    end.

%% @spec handle_reply_b(reply(), check()) -> {bin, binary()} | {fail, json()} | {error, reason()}
%% @doc Internal function for handling binary http:request.
%% @type check() = string(). Status code to to distinguish between ok and fail states.
handle_reply_b(Reply, Check) ->
    case Reply of
        {error, Reason} ->
            {error, Reason};
        {ok, {{_, Status, _}, _Headers, Body}} ->
            case Status of
                Check -> {bin, Body};
                _ ->
                    try engejson:decode(Body) of
                        Json -> {fail, Json}
                    catch
                        _Error:Reason -> {error, Reason}
                    end
            end
    end.

query_string(Options) ->
    query_string(Options, "?", []).
query_string([], _Separator, Acc) ->
    lists:flatten(lists:reverse(Acc));
query_string([{Name, Value} | T], Separator, Acc) when is_integer(Value) ->
    query_string([{Name, integer_to_list(Value)} | T], Separator, Acc);
query_string([{Name, Value} | T], Separator, Acc) ->
    O = lists:flatten(io_lib:format("~s~s=~s", [Separator, Name, Value])),
    query_string(T, "&", [O | Acc]).

http_p_request(Method, Url, Body) ->
    http_p_request(Method, Url, Body, "application/json").
http_p_request(Method, Url, Doc, ContentType) ->
    Reply = http:request(Method, {Url, [], ContentType, list_to_binary(Doc)}, [], [?BINARY]),
    handle_reply(Reply, 201).
http_g_request(Url) ->
    Reply = http:request(get, {Url, []}, [], [?BINARY]),
    handle_reply(Reply, 200).
http_b_request(Url) ->
    Reply = http:request(get, {Url, []}, [], [?BINARY]),
    handle_reply_b(Reply, 200).
http_d_request(Url) ->
    Reply = http:request(delete, {Url, []}, [], [?BINARY]),
    handle_reply(Reply, 200).
