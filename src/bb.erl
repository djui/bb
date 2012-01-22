-module(bb).

%%%_* Exports ==========================================================
-export([ call/2
        , get_node/1
        , start/0
        ]).

%%%_* Imports ==========================================================
-import(bb_util, [ b/1
                 , env/1
                 , kf/2
                 , kf/3
                 , s/1
                 ]).

%%%_* Defines ==========================================================
-define(SERVER_URI,       env(server_uri)).
-define(SERVER_DATA_PATH, env(server_data_path)).
-define(USER_ME,          env(user_me)).
-define(API,              (neo4j_api:new(?SERVER_URI++?SERVER_DATA_PATH))).
%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
start() ->
  bb_util:ensure_started(bb),
  ensure_server_running().

call(Method, Args) ->
  erlang:apply(?API, Method, Args).

get_node(NodeId) ->
  {ok, Node} = ?API:getNode(NodeId),
  Node.

get_node_props(NodeId) ->
  {ok, NodeProps} = ?API:getAllNodeProperties(NodeId),
  NodeProps.

create_node(NodeProps) ->
  {ok, Node} = ?API:createNode(NodeProps),
  {ok, NodeId} = ?API:getNodeId(Node),
  NodeId.

%% TODO
%% create_node(NodeProps, ConnectNodeId) ->
%%  create_node(NodeProps

get_rel(RelId) ->
  {ok, Rel} = ?API:getRelationship(RelId),
  Rel.

get_rel_props(RelId) ->
  {ok, RelProps} = ?API:getAllRelationshipProperties(RelId),
  RelProps.

get_node_rel(NodeId, RelDir) ->
  {ok, NodeRel} = ?API:getNodeRelationships(NodeId, RelDir),
  NodeRel.

create_node_rel(NodeId, RelProps) ->
  {ok, Rel} = ?API:createRelation(NodeId, RelProps),
  Rel.

%% connect_nodes(FromId, ToId) -> nyi.

%%%_* Internals --------------------------------------------------------
ensure_server_running() ->
  case ?API:getRoot() of
    {ok, Config} ->
      Version = kf(b("neo4j_version"), Config),
      case Version =:= b(?API:version()) of
        true  -> ok;
        false -> erlang:error(incorrect_version)
      end;
    {error, Error} -> erlang:error(Error)
  end.

%%%_* Helpers ----------------------------------------------------------
kf(Key, List) -> kf(Key, List, undefined).

kf(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Value} -> Value;
    false        -> Default
  end.

b(B) when is_binary(B) -> unicode:characters_to_list(B);
b(S) when is_list(S)   -> unicode:characters_to_binary(S).

%%% Mode: Erlang
%%% End.
