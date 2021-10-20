%% Taken from gleam_erlang library
-module(atom_ffi).

-export([atom_from_string/1]).

-spec atom_from_string(binary()) -> {ok, atom()} | {error, atom_not_loaded}.
atom_from_string(S) ->
  try
    {ok, binary_to_existing_atom(S, utf8)}
  catch
    error:badarg ->
      {error, atom_not_loaded}
  end.
