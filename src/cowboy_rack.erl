%%%-------------------------------------------------------------------
%%% @author Jack Tang <jack@taodi.local>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 13 Nov 2014 by Jack Tang <jack@taodi.local>
%%%-------------------------------------------------------------------
-module(cowboy_rack).

%% API
-export([start/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(?MODULE).

%%%===================================================================
%%% Internal functions
%%%===================================================================
