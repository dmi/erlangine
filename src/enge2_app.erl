%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the enge2 application.

-module(enge2_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for enge2.
start(_Type, _StartArgs) ->
    enge2_deps:ensure(),
    enge2_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for enge2.
stop(_State) ->
    ok.
