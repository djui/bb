-module(bb_helper).

%%%_* Exports ==========================================================
-export([ b/1
        , ensure_started/1
        , path/1
        , env/1
        , kf/2
        , kf/3
        , priv_file/1
        , stringify/1
        ]).

%%%_* Defines ==========================================================

%%%_* Code =============================================================
%%%_* API --------------------------------------------------------------
ensure_started(Application) ->
  ensure_started(Application, application:start(Application)).

ensure_started(_Application, ok) ->
  ok;
ensure_started(_Application, {error, {already_started, _Application}}) ->
  ok;
ensure_started(Application, {error, {not_started, Dependency}}) ->
  ok = ensure_started(Dependency),
  ensure_started(Application);
ensure_started(Application, {error, Reason}) ->
  erlang:error({app_start_failed, Application, Reason}).

path(L) -> filename:join(lists:map(fun stringify/1, L)).

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

b(B) when is_binary(B) -> unicode:characters_to_list(B);
b(L) when is_list(L)   -> unicode:characters_to_binary(L).

env(Key) ->
  {ok, Value} = application:get_env(bb, Key),
  Value.

priv_file(Filename) ->
  %% PrivDir = code:priv_dir(bb),
  %% filename:join(PrivDir, Filename).
  filename:join([ filename:dirname(code:which(?MODULE))
                , ".."
                , "priv"
                , Filename
                ]).

%%% Mode: Erlang
%%% End.
