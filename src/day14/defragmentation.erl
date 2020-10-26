-module(defragmentation).
-export([part1/0]).
-define(PUZZLE_INPUT, "oundnydw").
-define(GRID_SIZE, 128).

create_bit_mapping() ->
  #{
    $0 => "0000",
    $1 => "0001",
    $2 => "0010",
    $3 => "0011",
    $4 => "0100",
    $5 => "0101",
    $6 => "0110",
    $7 => "0111",
    $8 => "1000",
    $9 => "1001",
    $a => "1010",
    $b => "1011",
    $c => "1100",
    $d => "1101",
    $e => "1110",
    $f => "1111"
  }.

create_row(Sequence, BitMapping) ->
  HashInput = ?PUZZLE_INPUT ++ "-" ++ integer_to_list(Sequence),
  Hash = knot_hash:knot(HashInput),
  lists:flatten([maps:get(Char, BitMapping) || Char <- Hash]).

create_grid() ->
  BitMapping = create_bit_mapping(),
  [create_row(S, BitMapping) || S <- lists:seq(0, ?GRID_SIZE - 1)].

count_squares_used() ->
  Grid = create_grid(),
  length(lists:filter(fun(Digit) -> Digit == $1 end, lists:flatten(Grid))).

part1() ->
  count_squares_used().
