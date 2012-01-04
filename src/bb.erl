-module(bb).

%%%_* Exports ==========================================================
-export([ start/0
        , get_node/1
        ]).

%%%_* Defines ==========================================================
-define(SERVER_URI,       bb_util:env(server_uri)).
-define(SERVER_DATA_PATH, bb_util:env(server_data_path)).
-define(USER_ME,          bb_util:env(user_me)).

%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
start() ->
  bb_util:ensure_started(bb),
  ensure_server_running().

get_node(me)                     -> get_node(?USER_ME);
get_node(Id) when is_integer(Id) ->
  API = neo4j_api:new(?SERVER_URI++?SERVER_DATA_PATH),
  {ok, Res} = API:getNode(bb_util:s(Id)),
  bb_util:kf(bb_util:b("data"), Res).

add_node(FromId, RelProps, NodeProps) when is_integer(FromId) ->
  Id  = create_node(NodeProps),
  Rel = create_rel(RelProps),
  ok  = connect_nodes(FromId, Id),
  {ok, Id}.

create_node(NodeProps) ->
  Body     = NodeProps,
  Payload  = mochijson2:encode(Body, [{format, proplist}]),
  Response = bb_rest:request(post, "node/"),
  Response.
  
create_rel(RelProps) -> nyi.

connect_nodes(FromId, ToId) -> nyi.

%%%_* Internals --------------------------------------------------------
ensure_server_running() ->
  {ok, Config} = rest_client:request(get, ?SERVER_URI),
  _DataPath    = bb_util:kf(bb_util:b("data"), Config).

%%% Mode: Erlang
%%% End.
