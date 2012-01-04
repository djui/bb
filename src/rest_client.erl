-module(rest_client).

%%%_* Exports ==========================================================
-export([ request/2
        , request/3
        ]).

%%%_* Defines ==========================================================
-define(SERVER_TIMEOUT, 3000).

%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
request(Method, Path) ->
  Request  = {Path, []},
  Response = send_request(Method, Request),
  parse_response(Response).

request(Method, Path, Data) ->
  Payload  = percent:url_encode(Data),
  Request  = {Path, [], "application/x-www-form-urlencoded", Payload},
  Response = send_request(Method, Request),
  parse_response(Response).

%%%_* Internals --------------------------------------------------------
send_request(Method, {Url, []}) ->
  httpc:request(Method, {Url, []}, [{timeout, ?SERVER_TIMEOUT}], []).
 
parse_response({ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}}) ->
  case kf("content-type", Headers) of
    "application/vnd.sun.wadl+xml" ->
      Schema      = priv_file("wadl.xsd"),
      {ok, Model} = erlsom:compile_xsd_file(Schema),
      {ok, _WADL} = erlsom:parse(Body, Model);
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      {ok, Payload};
    _ ->
      parse_response({error, invalid_content_type})
  end;
parse_response({ok, {{"HTTP/1.1", 404, "Not Found"}, Headers, Body}}) ->
  case kf("content-type", Headers) of
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      Message = kf(b("message"), Payload),
      {error, Message};
    _ ->
      parse_response({error, invalid_method})
  end;
parse_response({error, _Reason}=Error) -> Error.

%%%_* Helpers ----------------------------------------------------------
kf(Key, List) ->
  {Key, Value} = lists:keyfind(Key, 1, List),
  Value.

b(B) when is_binary(B) -> unicode:characters_to_list(B);
b(S) when is_list(S)   -> unicode:characters_to_binary(S).

priv_file(Filename) ->
  filename:join([ filename:dirname(code:which(?MODULE)), "../priv", Filename]).

%%% Mode: Erlang
%%% End.
