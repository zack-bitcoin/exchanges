-module(exchanges_sup).

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

init([]) ->
    ssl:start(),
    application:start(inets),
    Children = 
	[
	 ?CHILD(prices, worker),
	 %{keys, {keys, start_link, []}, permanent, 5000, worker, [keys]},
	 ?CHILD(keys, worker),
	 ?CHILD(password, worker)
	],
    {ok, { {one_for_one, 5, 10}, Children} }.

