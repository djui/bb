%% TODO Do standard wsdl parsing
-module(bb).

-export([ start/0
        , get_node/1
        , rest/1
        , rest/2
        , rest/3
        ]).


start() ->
  inets:start(),
  {ok, Res} = rest(""),
  ServerConfig = kf(<<"data">>, Res),
  erlang:put(bb_server_path, erlang:binary_to_list(ServerConfig)),
  ok.

get_node(Id) when is_integer(Id) ->
  {ok, Res} = rest("node/"++erlang:integer_to_list(Id)),
  kf(<<"data">>, Res).

rest(Path) ->
  rest(get, Path).

rest(Method, Path) ->
  Url = server_path() ++ Path,
    case httpc:request(Method, {Url, []}, [], []) of
      {ok,{{"HTTP/1.1",200,"OK"},_Headers,Body}} ->
        Response = mochijson2:decode(Body, [{format, proplist}]),
        {ok, Response};
      {ok,{{"HTTP/1.1",404,"Not Found"},_Headers,Body}} ->
        Response = mochijson2:decode(Body, [{format, proplist}]),
        Message = kf(<<"message">>, Response),
        {error, Message}
    end.

rest(Method, Path, _Payload) ->
  Url = server_path() ++ Path,
    case httpc:request(Method, {Url, []}, [], []) of
      {ok,{{"HTTP/1.1",200,"OK"},_Headers,Body}} ->
        Response = mochijson2:decode(Body, [{format, proplist}]),
        {ok, Response};
      {ok,{{"HTTP/1.1",404,"Not Found"},_Headers,Body}} ->
        Response = mochijson2:decode(Body, [{format, proplist}]),
        Message = kf(<<"message">>, Response),
        {error, Message}
    end.

server_path() ->
  case erlang:get(bb_server_path) of
    undefined ->
      {Protocol, Host, Port, Path} = default_path(),
      Protocol++"://"++Host++":"++erlang:integer_to_list(Port)++Path;
    Config ->
      Config
  end.

default_path() ->
  {"http", "localhost", 7474, "/"}.

kf(Key, List) ->
  {Key, Value} = lists:keyfind(Key, 1, List),
  Value.

kf(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    false        -> Default;
    {Key, Value} -> Value
  end.
