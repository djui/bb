-module(bb_rest).

%%%_* Exports ==========================================================
-export([ init/0
        , create_wadl_hrl/0
        ]).

-ignore_xref([ create_wadl_hrl/0 ]).

%%%_* Includes =========================================================
-include_lib("bb/include/wadl.hrl").

%%%_* Defines ==========================================================
-define(SERVER_URI,       bb_util:env(server_uri)).
-define(SERVER_DATA_PATH, bb_util:env(server_data_path)).
-define(SERVER_TIMEOUT,   bb_util:env(server_timeout)).

%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
init() ->
  ensure_server_running(),
  hotload_wadl(parse_wadl(get_wadl())).

request(Method, Path) ->
  Request  = {abs_path(Path), []},
  Response = send_request(Method, Request),
  parse_response(Response).

request(Method, Path, Data) ->
  Payload  = percent:url_encode(Data),
  Request  = {abs_path(Path), [], "application/x-www-form-urlencoded", Payload},
  Response = send_request(Method, Request),
  parse_response(Response).

%%%_* Internals --------------------------------------------------------
ensure_server_running() ->
  {ok, Config} = request(get, "/"),
  _DataPath    = bb_util:kf(bb_util:b("data"), Config).

send_request(Method, {Path, []}) ->
  Url = ?SERVER_URI++Path,
  httpc:request(Method, {Url, []}, [{timeout, ?SERVER_TIMEOUT}], []).
 
parse_response({ok, {{"HTTP/1.1", 200, "OK"}, Headers, Body}}) ->
  case bb_util:kf("content-type", Headers) of
    "application/vnd.sun.wadl+xml" ->
      Schema      = bb_util:priv_file("wadl.xsd"),
      {ok, Model} = erlsom:compile_xsd_file(Schema),
      {ok, _WADL} = erlsom:parse(Body, Model);
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      {ok, Payload};
    _ ->
      parse_response({error, invalid_content_type})
  end;
parse_response({ok, {{"HTTP/1.1", 404, "Not Found"}, Headers, Body}}) ->
  case bb_util:kf("content-type", Headers) of
    "application/json" ->
      Payload = mochijson2:decode(Body, [{format, proplist}]),
      Message = bb_util:kf(bb_util:b("message"), Payload),
      {error, Message};
    _ ->
      parse_response({error, invalid_method})
  end;
parse_response({error, _Reason}=Error) -> Error.

abs_path([$/|Path]) -> Path;
abs_path(Path)      -> filename:join(?SERVER_DATA_PATH, Path).

%%%_* WADL -------------------------------------------------------------
get_wadl() ->
  {ok, WADL} = request(get, "application.wadl"),
  WADL.

parse_wadl(WADL) ->
  collect_application(WADL).

hotload_wadl(WADL) ->
  Module   = bb_rest_api,
  BeamDir  = filename:dirname(code:which(?MODULE)),
  Filename = bb_util:path([BeamDir, Module]), %% or "generated" ?
  Binary   = <<"???">>,
  %{module, Module} = code:load_binary(Module, Filename, Binary),
  lists:foreach(fun format_api/1, WADL),
  WADL.

format_api({Name, Method, Path, [], []}) ->
  io:format("~p() ->~n", [Name]),
  io:format("  bb_rest:request(~p, ~p).~n", [Method, Path]);
format_api({Name, Method, Path, [], _ReqParams}) ->
  io:format("~p(Data) ->~n", [Name]),
  io:format("  bb_rest:request(~p, ~p, Data).~n", [Method, Path]);
format_api({Name, Method, Path0, ResParams, []}) ->
  Params = format_params(ResParams),
  Path   = format_path(Path0, ResParams),
  io:format("~p(~p) ->~n", [Name, string:join(Params, ", ")]),
  io:format("  bb_rest:request(~p, ~p).~n", [Method, Path]);
format_api({Name, Method, Path0, ResParams, _ReqParams}) ->
  Params = format_params(ResParams),
  Path   = format_path(Path0, ResParams),
  io:format("~p(~p, Data) ->~n", [Name, string:join(Params, ", ")]),
  io:format("  bb_rest:request(~p, ~p, Data).~n", [Method, Path]).

format_params(Params) ->
  ["P"++bb_util:s(N) || N <- lists:seq(1, length(Params))].

format_path(Path, Params) ->
  Fun = fun({Key, Value}, P) -> re:replace(P, "{"++Key++"}", Value) end,
  lists:foldl(Fun, Path, Params).

collect_application(#application{resources=Resources}) ->
  lists:flatten(collect_resources(Resources)).

collect_resources([]) -> [];
collect_resources([#resources{ base=Base
                             , resource=Resource}|T]) ->
  Resources = collect_resource(Base, Resource),
  [Resources|collect_resources(T)].

collect_resource(_,    []) -> [];
collect_resource(Base, [#resource{ path=Path
                                 , param=Params
                                 , choice=Choices}|T]) ->
  Resource = collect_resource(join_path(Base, Path), Params, Choices),
  [Resource|collect_resource(Base, T)].

collect_resource(_,    _,      []) -> [];
collect_resource(Path, Params, [#resource{}=H|T]) ->
  Resource = collect_resource(Path, [H]),
  [Resource|collect_resource(Path, Params, T)];
collect_resource(Path, Params, [#method{ id=Id
                                        , name=Name
                                        , request=Request}|T]) ->
  ResourceParams = collect_param(Params),
  RequestParams  = collect_request(Request),
  Resource = { erlang:list_to_atom(Id)
             , map_method(Name)
             , Path
             , ResourceParams
             , RequestParams
             },
  [Resource|collect_resource(Path, Params, T)].

collect_request(undefined) -> [];
collect_request(#request{param=Param}) ->
  collect_param(Param).

collect_param([]) -> [];
collect_param(undefined) -> [];
collect_param([#param{style="query", name=Name, type=Type}|T]) ->
  [{Name, map_type(Type)}|collect_param(T)];
collect_param([#param{style="template", name=Name, type=Type}|T]) ->
  [{Name, map_type(Type)}|collect_param(T)].

%% FIXME Should be a record, but wasn't generated by erlsom.
map_type({qname, _href, "int",    "xs", "xsd"}) -> integer;
map_type({qname, _href, "long",   "xs", "xsd"}) -> integer;
map_type({qname, _href, "string", "xs", "xsd"}) -> string.

%% map_method("OPTIONS") -> options;
map_method("GET")     -> get;
%% map_method("HEAD")    -> head;
map_method("POST")    -> post;
map_method("PUT")     -> put;
map_method("DELETE")  -> delete.
%% map_method("TRACE")   -> trace.
%% map_method("CONNECT") -> connect.

%%%_* Helpers ----------------------------------------------------------
join_path(Path1, Path2) ->
  string:strip(Path1, right, $/) ++ "/" ++ string:strip(Path2, left, $/).

create_wadl_hrl() ->
  SchemaFilename = bb_util:priv_file("wadl.xsd"),
  RecDefFilename = "include/wadl.hrl",
  erlsom:write_xsd_hrl_file(SchemaFilename, RecDefFilename).
      
%%% Mode: Erlang
%%% End.
