
-module(dojoApp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Params) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 200,
    Type = worker,

    Network = {dojoNetwork, {dojoNetwork, start_link, []},
      Restart, Shutdown, Type, [dojoRegion]},

    Manager = {dojoManager, {dojoManager, start_link, []},
      Restart, Shutdown, Type, [dojoManager]},

    {ok, {SupFlags, [Manager, Network]}}.

