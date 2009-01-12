%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the enge2 application.

-module(enge2_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,   
    WebConfig = [
         {ip, Ip},
                 {port, 8080},
                 {docroot, enge2_deps:local_path(["priv", "www"])}],

    Web = {enge2_web,
           {enge2_web, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

    AuthDB = {authdb,
              {authdb, start, []},
	      permanent, 5000, worker, [authdb]},

    RealmDB = {realmdb,
              {realmdb, start, []},
	      permanent, 5000, worker, [realmdb]},

    Session = {session,
               {session, start, []},
	       permanent, 5000, worker, [session]},

    Processes = [Web, AuthDB, RealmDB, Session],
    {ok, {{one_for_one, 10, 10}, Processes}}.
