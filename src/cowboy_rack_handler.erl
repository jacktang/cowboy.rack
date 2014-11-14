%%%-------------------------------------------------------------------
%%% @author Jack Tang <himars@gmail.com>
%%% @copyright (C) 2014, Jack Tang
%%% @doc
%%%
%%% @end
%%% Created : 14 Nov 2014 by Jack Tang <jack@taodi.local>
%%%-------------------------------------------------------------------
-module(cowboy_rack_handler).

%% API
-export([init/2, terminate/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init(Req, Opt) ->
    Req2 = handle(Req, Opt),
    {ok, Req2, Opt}.


terminate(_Reason, _Req, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle(Req, Opt) ->
    Path = proplists:get_value(path, Options, "./priv"),
    case file:read_file_info(filename:join(Path, "config.ru")) of
    {ok, _Info} ->
            {ok, {Status, ReplyHeaders, ReplyBody}, Req1} = handle(Req, {path, Path}),
            case proplists:get_value(<<"X-Accel-Redirect">>, ReplyHeaders) of
                undefined ->
                    cowboy_req:reply(Status, ReplyHeaders, ReplyBody, Req1);
                Redirect ->
                    lager:debug("Rack redirect: ~p", [Redirect]),              
                    {unhandled, Req, lists:keystore(path, 1, Env, {path, Redirect})}
            end;
        {error, _} ->
            unhandled
  end.

handle(Req, {path, Path}) when is_list(Path) ->
  handle(Req, {path, list_to_binary(Path)});

  
handle(Req, {path, Path}) when is_binary(Path) ->  
    {RequestMethod, Req1} = cowboy_req:method(Req),
    {ScriptName, Req2} = cowboy_req:path(Req1),
    {_PathInfo, Req3} = cowboy_req:path_info(Req2),
    {QueryString, Req4} = cowboy_req:qs(Req3),
    {ServerName, Req5} = cowboy_req:host(Req4),
    {ServerPort, Req6} = cowboy_req:port(Req5),
    {RequestHeaders, _} = cowboy_req:headers(Req6),
    {ok, Body, Req7} = case RequestMethod of
                           <<"POST">> -> cowboy_req:body(Req6);
                           _ -> {ok, <<"">>, Req6}
                       end,
    
    % Trying to follow http://rack.rubyforge.org/doc/SPEC.html here 
    RackSession = [
                   {<<"REQUEST_METHOD">>, RequestMethod},%atom_to_binary(RequestMethod, latin1)},
                   {<<"SCRIPT_NAME">>, <<"">>}, %join(lists:sublist(ScriptName, length(ScriptName) - length(PathInfo)), <<"/">>)},
                   {<<"PATH_INFO">>, ScriptName}, %join(PathInfo, <<"/">>)},
                   {<<"QUERY_STRING">>, QueryString},
                   {<<"SERVER_NAME">>, ServerName}, %join(ServerName, ".")},
                   {<<"SERVER_PORT">>, list_to_binary(integer_to_list(ServerPort))},
                   {<<"HTTP_HOST">>, <<ServerName/binary, ":", (list_to_binary(integer_to_list(ServerPort)))/binary>>}%<<(join(ServerName, "."))/binary, ":", (list_to_binary(integer_to_list(ServerPort)))/binary>>}
                  ] ++ translate_headers(RequestHeaders),
  
    % io:format("************************~nRACK:~n~p~n************************~n", [RackSession]),

    io:format("~n================~nREQUEST:~n==================Path:~n~p~nSession:~n~p~nBody:~n~p~n~n~n~n~n", [Path, RackSession, Body]),

    case rack:request(Path, RackSession, Body) of
        {ok, {_Status, _ReplyHeaders, _ReplyBody} = Reply} ->
            % ?D({_Status, RequestMethod, join(PathInfo, <<"/">>), iolist_size(_ReplyBody)}),
            {ok, Reply, Req7};
        {error, busy} ->
            {ok, {503, [], <<"Backend overloaded\r\n">>}, Req7};
        {error, timeout} ->
            {ok, {504, [], <<"Backend timeout\r\n">>}, Req7};
        {error, Error} ->
            {ok, {500, [], iolist_to_binary(io_lib:format("Internal server error: ~p\r\n", [Error]))}, Req7}
    end.

