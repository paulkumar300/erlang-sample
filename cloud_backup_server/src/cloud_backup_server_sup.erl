%%%-------------------------------------------------------------------
%% @doc cloud_backup_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cloud_backup_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->

	Cloud_server = { cloud_server, { cloud_server, start_link, []}, permanent, 2000, worker, [cloud_server]},

    {ok, { {one_for_one, 5, 10}, [ Cloud_server ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
