-module(bb_rest_api).

%%%_* Exports ==========================================================
-export([ performBatchOperations/2
        , getExtensionsList/0
        ]).

%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
performBatchOperations(ResourceParams, RequestParams) ->
  bb_rest:request(post, "http://localhost:7474/db/data/batch", []).

getExtensionsList() ->
  bb_rest:request(get, "http://localhost:7474/db/data/ext", []).

getExtensionList(Name) ->
  Method = get,
  Path0 = "http://localhost:7474/db/data/ext/{name}",
  Path  = re:replace(Path0, "{name}", Name, [{return, list}]),
  bb_rest:request(get, Path).

getExtensionList(ResourceParams, RequestParams) ->
  Method = get,
  Path0 = "http://localhost:7474/db/data/ext/{name}",
  [{Key, Value}] = ResourceParams,
  Path  = re:replace(Path0, "{"++Key++"}", Value, [{return, list}]),
  bb_rest:request(Method, Path).

%%%_* Internals --------------------------------------------------------
api() ->
  [ { performBatchOperations
    , post
    , "http://localhost:7474/db/data/batch"
    , []
    , []
    }
  , { getExtensionsList
    , get
    , "http://localhost:7474/db/data/ext"
    , []
    , []
    }
  , { getExtensionList
    , get
    , "http://localhost:7474/db/data/ext/{name}"
    , [{"name",string}]
    , []
    }
  , { invokeGraphDatabaseExtension
    , post
    , "http://localhost:7474/db/data/ext/{name}/graphdb/{method}"
    , [{"name",string},{"method",string}]
    , []
    }
  , { getGraphDatabaseExtensionDescription
    , get
    , "http://localhost:7474/db/data/ext/{name}/graphdb/{method}"
    , [{"name",string},{"method",string}]
    , []
    }
  , { invokeNodeExtension
    , post
    , "http://localhost:7474/db/data/ext/{name}/node/{nodeId}/{method}"
    , [{"nodeId",integer},{"name",string},{"method",string}]
    , []
    }
  ].
   
%%% Mode: Erlang
%%% End.
