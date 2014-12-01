%%%-------------------------------------------------------------------
%%% @author Jack Tang <himars@gmail.com>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 14 Nov 2014 by Jack Tang <jack@taodi.local>
%%%-------------------------------------------------------------------
-module(cowboy_rack_handler).
-define(APP, cowboy_rack). 
%% API
-export([init/2, terminate/3, info/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init(Req, Opt) ->
    TimeOut = application:get_env(?APP, req_timeout, 5000),
    handle(Req, {options, Opt}),
    {cowboy_loop, Req, Opt, TimeOut, hibernate}.

info({reply, Response}, Req, State) ->
    % lager:info(Response),
    {Status, Headers, Body} = Response,
    Req2 = cowboy_req:reply(Status, Headers, Body, Req),
    {stop, Req2, State};
info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
  ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
handle(Req, {options, Opt}) ->
    Path = proplists:get_value(path, Opt, "./priv"),
    case file:read_file_info(filename:join(Path, "config.ru")) of
    {ok, _Info} ->
        handle(Req, {path, Path});
    {error, _} ->
        unhandled
  end;

handle(Req, {path, Path}) when is_list(Path) ->
  handle(Req, {path, list_to_binary(Path)});

  
handle(Req, {path, Path}) when is_binary(Path) ->
    RequestMethod = cowboy_req:method(Req),
    ScriptName = cowboy_req:path(Req),
    % _PathInfo = cowboy_req:path_info(Req),
    QueryString = cowboy_req:qs(Req),
    ServerName = cowboy_req:host(Req),
    ServerPort = cowboy_req:port(Req),
    RequestHeaders = cowboy_req:headers(Req),
    {ok, Body, _} = case RequestMethod of
                        <<"POST">> -> cowboy_req:body(Req);
                        _ -> {ok, <<"">>, Req}
                    end,
    RackSession = [
                   {<<"REQUEST_METHOD">>, RequestMethod},
                   {<<"SCRIPT_NAME">>, <<"">>}, 
                   {<<"PATH_INFO">>, ScriptName}, 
                   {<<"QUERY_STRING">>, QueryString},
                   {<<"SERVER_NAME">>, ServerName}, 
                   {<<"SERVER_PORT">>, list_to_binary(integer_to_list(ServerPort))},
                   {<<"HTTP_HOST">>, <<ServerName/binary, ":", (list_to_binary(integer_to_list(ServerPort)))/binary>>}
                  ] ++ translate_headers(RequestHeaders),
    gen_server:cast(cowboy_rack_req_pool, {request, self(), RackSession, Body}).            


translate_headers(Headers) ->
    lists:foldl(fun({'Host', _}, Acc) ->
                        Acc;
                   ({K,V}, Acc) when is_binary(K) ->
                        Name = "HTTP_" ++ re:replace(string:to_upper(binary_to_list(K)), "\\-", "_"),
                        [{list_to_binary(Name), V}|Acc];
                   ({K,V}, Acc) when is_atom(K) ->
                        Name = "HTTP_" ++ re:replace(string:to_upper(atom_to_list(K)), "\\-", "_"),
                        [{list_to_binary(Name), V}|Acc]
                end, [], Headers).
