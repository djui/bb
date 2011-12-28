-module(bb).

%%%_* Exports ==========================================================
-export([ start/0
        , get_node/1
        ]).

%%%_* Defines ==========================================================
-define(USER_ME, bb_helper:env(user_me)).

%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
start() ->
  bb_helper:ensure_started(bb),
  bb_rest:init().

get_node(me)                     -> get_node(?USER_ME);
get_node(Id) when is_integer(Id) ->
  {ok, Res} = bb_rest:request(get, ["node", Id]),
  bb_helper:kf(bb_helper:b("data"), Res).

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

%%% Mode: Erlang
%%% End.
