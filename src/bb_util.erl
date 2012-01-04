-module(bb_util).

%%%_* Exports ==========================================================
-export([ a/1
        , b/1
        , ensure_started/1
        , path/1
        , env/1
        , kf/2
        , kf/3
        , priv_file/1
        , s/1
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

path(L) -> filename:join(lists:map(fun s/1, L)).

kf(Key, List) ->
  {Key, Value} = lists:keyfind(Key, 1, List),
  Value.

kf(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    false        -> Default;
    {Key, Value} -> Value
  end.

a(S) when is_list(S)    -> erlang:list_to_atom(S);
a(N) when is_integer(N) -> a(s(N));
a(A) when is_atom(A)    -> A.

s(S) when is_list(S)    -> S;
s(N) when is_integer(N) -> erlang:integer_to_list(N);
s(A) when is_atom(A)    -> erlang:atom_to_list(A);
s(B) when is_binary(B)  -> unicode:characters_to_list(B).

b(B) when is_binary(B) -> unicode:characters_to_list(B);
b(S) when is_list(S)   -> unicode:characters_to_binary(S).

env(Key) ->
  {ok, Value} = application:get_env(bb, Key),
  Value.

priv_file(Filename) ->
  filename:join([ filename:dirname(code:which(?MODULE)), "../priv", Filename]).

%%% Mode: Erlang
%%% End.
