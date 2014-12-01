%%%-------------------------------------------------------------------
%%% @author Jack Tang <himars@gmail.com>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 14 Nov 2014 by Jack Tang <himars@gmail.com>
%%%-------------------------------------------------------------------
-module(cowboy_rack_req_pool).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(APP, cowboy_rack). 

-record(state, {worker_pool_num, queue}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
% start() ->
%     case supervisor:start_child(cowboy_rack_req_pool_sup, []) of
%         {ok, Pid}           -> {ok, Pid};
%         {ok, Pid, _Info}    -> {ok, Pid};
%         {error, {already_started, Pid}} -> {ok, Pid};
%         {error, Reason}     -> {error, Reason}
%     end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(WorkerPoolNum) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [WorkerPoolNum], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([WorkerPoolNum]) ->
    gen_server:cast(self(), {spawn, WorkerPoolNum}),
  {ok, #state{queue = queue:new(), worker_pool_num = WorkerPoolNum}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    lager:error("Can't handle request: ~p", [_Request]),
    {reply, {error, invalid_request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({spawn, WorkerPoolNum}, State) ->
    pg2:create(?MODULE),
    lists:foreach(fun(_N) ->
                    case cowboy_rack_worker:start() of
                        {ok, Pid} ->
                            pg2:join(?MODULE, Pid);
                        _ ->
                            lager:error("cowboy_rack_worker process create faild"),    
                            faild
                    end
                   end, lists:seq(1, WorkerPoolNum)
                 ),
    {noreply, State};
handle_cast({request, From, Headers, Body}, #state{queue = RequestQueue} = State) ->
    gen_server:cast(self(), standby),
    {noreply, State#state{queue = queue:in({From, Headers, Body}, RequestQueue)}};
handle_cast(standby, #state{queue = RequestQueue} = State) ->
    case pg2:get_closest_pid(?MODULE) of
        {error, _} ->
            {noreply, State};
        Pid ->
            pg2:leave(?MODULE, Pid),
            case queue:out(RequestQueue) of
                {{value, Request}, RequestQueue2} -> 
                    {From, Headers, Body} = Request,
                    gen_server:cast(Pid, {request, {From, Headers, Body}}), 
                    {noreply, State#state{queue = RequestQueue2}};
                {empty, _} ->
                    pg2:join(?MODULE, Pid),
                    {noreply, State}
             end
    end;        
handle_cast(_Msg, State) ->
    lager:error("Can't handle msg: ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
