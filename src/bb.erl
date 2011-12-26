%%% TODO Do standard wsdl parsing
%%% TODO Maybe use ibrowse for http handling https://github.com/cmullaparthi/ibrowse/
-module(bb).

%%%_* Exports ==========================================================
-export([ start/0
        , get_node/1
        ]).

%%%_* Defines ==========================================================
-define(SERVER_URI,       env(server_uri)).
-define(SERVER_DATA_PATH, env(server_data_path)).
-define(SERVER_TIMEOUT,   env(server_timeout)).
-define(USER_ME,          env(user_me)).

%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
start() ->
  ensure_started(inets),
  ensure_started(bb),
  ensure_server_running().

get_node(me)                     -> get_node(?USER_ME);
get_node(Id) when is_integer(Id) ->
  {ok, Res} = request(get, normalize(["node", Id]),
  kf(b("data"), Res).

%%%_* Internals --------------------------------------------------------
ensure_server_running() ->
  {ok, Config} = request(get, "/"),
  _DataPath    = kf(b("data"), Config).

request(Method, Path) ->
  Request  = {abs_path(Path), []},
  Response = send_request(Method, Request),
  parse_response(Response).

request(Method, Path, Data) ->
  Payload  = percent:url_encode(Data),
  Request  = {abs_path(Path), [], "application/x-www-form-urlencoded", Payload},
  Response = send_request(Method, Request),
  parse_response(Response).

send_request(Method, {Path, []}) ->
  Url = ?SERVER_URI ++ Path,
  httpc:request(Method, {Url, []}, [{timeout, ?SERVER_TIMEOUT}], []).
 
parse_response({ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Body}}) ->
  Payload = mochijson2:decode(Body, [{format, proplist}]),
  {ok, Payload};
parse_response({ok, {{"HTTP/1.1", 404, "Not Found"}, Headers, Body}}) ->
  case kf("content-type", Headers) of
    "application/json" ->
      io:format("BODY: ~p~n", [Body]),
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      Message = kf(b("message"), Payload),
      {error, Message};
    _ ->
      parse_response({error, invalid_method})
  end;
parse_response({error, _Reason}=Error) -> Error.

path(L) -> filename:join(lists:map(stringify/1, L)).

abs_path("/"++Path) -> Path;
abs_path(Path)      -> ?SERVER_DATA_PATH++Path.

%%%_* Helpers ----------------------------------------------------------
ensure_started(App) ->
  case application:start(App) of
    ok                              -> ok;
    {error, {already_started, App}} -> ok;
    {error, _}=Error                -> throw(Error)
  end.

stringify(S) when is_list(S)    -> S;
stringify(N) when is_integer(N) -> erlang:integer_to_list(N);
stringify(A) when is_atom(A)    -> erlang:atom_to_list(A);
stringify(B) when is_binary(B)  -> unicode:characters_to_list(B).

kf(Key, List) ->
  {Key, Value} = lists:keyfind(Key, 1, List),
  Value.

kf(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    false        -> Default;
    {Key, Value} -> Value
  end.

env(Key) ->
  {ok, Value} = application:get_env(bb, Key),
  Value.

b(B) when is_binary(B) -> unicode:characters_to_list(B);
b(L) when is_list(L)   -> unicode:characters_to_binary(L).

%%% Mode: Erlang
%%% End.
