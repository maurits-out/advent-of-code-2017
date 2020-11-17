-module(turing).
-export([diagnostics_checksum/0]).
-define(STEPS, 12656374).

turing_blueprint() ->
  #{
    {$A, 0} => {1, 1, $B},
    {$A, 1} => {0, -1, $C},
    {$B, 0} => {1, -1, $A},
    {$B, 1} => {1, -1, $D},
    {$C, 0} => {1, 1, $D},
    {$C, 1} => {0, 1, $C},
    {$D, 0} => {0, -1, $B},
    {$D, 1} => {0, 1, $E},
    {$E, 0} => {1, 1, $C},
    {$E, 1} => {1, -1, $F},
    {$F, 0} => {1, -1, $E},
    {$F, 1} => {1, 1, $A}
  }.

get_rule(State, Pos, Ones, TuringBlueprint) ->
  Value = case sets:is_element(Pos, Ones) of
            false -> 0;
            true -> 1
          end,
  maps:get({State, Value}, TuringBlueprint).

write_value(Pos, Ones, Value) ->
  case Value of
    0 -> sets:del_element(Pos, Ones);
    1 -> sets:add_element(Pos, Ones)
  end.

diagnostics_checksum(_, _, Ones, _, ?STEPS) ->
  sets:size(Ones);
diagnostics_checksum(CurrentState, CurrentPos, Ones, TuringBlueprint, Steps) ->
  {Value, Move, State} = get_rule(CurrentState, CurrentPos, Ones, TuringBlueprint),
  UpdatedOnes = write_value(CurrentPos, Ones, Value),
  diagnostics_checksum(State, CurrentPos + Move, UpdatedOnes, TuringBlueprint, Steps + 1).

diagnostics_checksum() ->
  Checksum = diagnostics_checksum($A, 0, sets:new(), turing_blueprint(), 0),
  io:format("The diagnostics checksum is ~p~n", [Checksum]).
