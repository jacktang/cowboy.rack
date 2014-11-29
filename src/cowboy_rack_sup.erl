%%%-------------------------------------------------------------------
%%% @author Jack Tang <himars@gmail.com>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 13 Nov 2014 by Jack Tang <himars@gmail.com>
%%%-------------------------------------------------------------------
-module(cowboy_rack_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(APP, cowboy_rack).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    % supervisor:start_link({local, ?SERVER}, ?MODULE, []).
    application_utils:start_supervisor(?MODULE, cowboy_rack_sup, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([cowboy_rack_sup]) ->
    RackEnv = application:get_env(?APP, rack_env, []),
    WorkerPoolNum = application:get_env(?APP, worker_pool_num, 20),
    CRWSup = application_utils:supervisor_spec(?MODULE, cowboy_rack_worker_sup, [RackEnv]),
    CRRPSup = application_utils:supervisor_spec(?MODULE, cowboy_rack_req_pool_sup, [WorkerPoolNum]),

    application_utils:one4one_supervisor([CRWSup]);

init([cowboy_rack_worker_sup, RackEnv]) ->
    RackWoker = application_utils:dynamic_child_spec(cowboy_rack_worker, [RackEnv]),
    application_utils:one4one_supervisor(simple, RackWoker).

init([cowboy_rack_req_pool_sup, WorkerPoolNum]) ->
    ReqPool = application_utils:child_spec(cowboy_rack_req_pool, [WorkerPoolNum]),
    application_utils:one4one_supervisor(ReqPool).


% init(_Args) ->
%     RestartStrategy = one_for_one,
%     MaxRestarts = 1000,
%     MaxSecondsBetweenRestarts = 3600,

%     SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
%     {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
