%%%-------------------------------------------------------------------
%%% @author Jack Tang <himars@gmail.com>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 13 Nov 2014 by Jack Tang <jack@himars@gmail.com>
%%%-------------------------------------------------------------------
-module(cowboy_rack_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
	ok = application:ensure_started(crypto),
	ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(ranch),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(cowboy),
    % ok = application:ensure_started(cowboy_rack),
    % cowboy_rack:start(),"wechat2.lktz.net"
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', cowboy_rack_handler, [{path, "/home/dev/Work/ninja"}]}
                ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 9900}],
                                [{env, [{dispatch, Dispatch}]}
                                ]),  

    case cowboy_rack_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
