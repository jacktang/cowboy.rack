%%%-------------------------------------------------------------------
%%% @author Jack Tang <himars@gmail.com>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 14 Nov 2014 by Jack Tang <himars@gmail.com>
%%%-------------------------------------------------------------------
-module(cowboy_rack_worker).

-behaviour(gen_server).

%% API
-export([start/1]).
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {port, rack_env, path, options, from}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec
%% @end
%%--------------------------------------------------------------------

start(Path) ->
    case supervisor:start_child(cowboy_rack_worker_sup, [Path]) of
        {ok, Pid}           -> {ok, Pid};
        {ok, Pid, _Info}    -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, Reason}     -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(RackEnv, Path) ->
    gen_server:start_link(?MODULE, [RackEnv, Path], []).

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
init([RackEnv, Path]) ->
    gen_server:cast(self(), launch_worker),
    {ok, #state{rack_env = RackEnv, path = Path}}.

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
handle_cast(launch_worker, #state{rack_env = RackEnv, path = Path} = State) ->
    WorkerPath = code:lib_dir(cowboy_rack, priv),
    Cmd = WorkerPath ++ "/worker.rb " ++ binary_to_list(Path),
    Port =               erlang:open_port({spawn, Cmd}, [ nouse_stdio,
                                               binary,
                                           exit_status,
                                           {packet,4},
                                           {env, RackEnv}
                                           ]),
    % Cmd = "rackup " ++ binary_to_list(Path) ++ "/config.ru",
    % Port = open_port({spawn, Cmd}, [{packet, 4},
    %                                 nouse_stdio,
    %                                 exit_status,
    %                                 binary%,
    %                                 %{env, RackEnv}
    %                                 ]), 
    {noreply, State#state{port = Port}};


handle_cast({request, From, Headers, Body}, #state{port = Port} = State) ->
    Packed = iolist_to_binary(
               [<<(length(Headers)):32>>,
                [<<(size(Key)):32,Key/binary,(size(Value)):32,Value/binary>> || {Key, Value} <- Headers],
                <<(size(Body)):32>>, Body]),
    port_command(Port, Packed),
    {noreply, State#state{from = From}};


handle_cast(_Msg, State) ->
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
handle_info({Port, {data, Bin}}, #state{from = From} = State) ->
     #state{port = Port, path = Path, from = From} = State,
    <<Status:32, HeadersCount:32, Rest/binary>> = Bin,
    {Headers, BodyType, RawBody} = extract_headers(Rest, HeadersCount, []),
    Body = case BodyType of
               file ->
                   {ok, B} = file:read_file(binary_to_list(RawBody)),
                   B;
               raw -> RawBody
           end,
    From ! {reply, {Status, Headers, Body}},
    gen_server:cast(cowboy_rack_req_pool, {release_pid, self()}),       
    {noreply, State};

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
extract_headers(<<BodyFlag, BodyLen:32, Body:BodyLen/binary>>, 0, Acc) ->
    BodyType = case BodyFlag of
                   1 -> file;
                   0 -> raw
               end,
    {lists:reverse(Acc), BodyType, Body};

extract_headers(<<KeyLen:32, Key:KeyLen/binary, ValueLen:32, Value:ValueLen/binary, Rest/binary>>,
                HeadersCount, Acc) ->
  extract_headers(Rest, HeadersCount - 1, [ {Key, Value} | Acc]).
