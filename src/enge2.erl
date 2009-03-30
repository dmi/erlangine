%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(enge2).
-author('author <author@example.com>').
-export([start/0, stop/0, prepare_db/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the enge2 server.
start() ->
    enge2_deps:ensure(),
    ensure_started(crypto),
    ensure_started(mnesia),
    ensure_started(inets),
    ensure_started(ecouch),
    enge2_conf:init(),
    application:start(enge2).

%% @spec stop() -> ok
%% @doc Stop the enge2 server.
stop() ->
    Res = application:stop(enge2),
    application:stop(ecouch),
    application:stop(inets),
    application:stop(mnesia),
    application:stop(crypto),
    Res.

prepare_db() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    authdb:reset(),
    realmdb:reset(),
    destination:reset().
