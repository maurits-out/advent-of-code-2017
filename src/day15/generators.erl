-module(generators).
-export([part1/0]).
-define(FACTOR_A, 16807).
-define(FACTOR_B, 48271).
-define(ITERATIONS, 40000000).

puzzle_input() ->
  {634, 301}.

part1() ->
  {StartA, StartB} = puzzle_input(),
  count_matching_pairs(StartA, StartB, 0, 0).

next(Value, Factor) ->
  (Value * Factor) rem 2147483647.

match(A, B) ->
  case (A band 16#ffff) == (B band 16#ffff) of
    true -> 1;
    false -> 0
  end.

count_matching_pairs(_, _, ?ITERATIONS, Acc) ->
  Acc;
count_matching_pairs(A, B, Count, Acc) ->
  NextA = next(A, ?FACTOR_A),
  NextB = next(B, ?FACTOR_B),
  count_matching_pairs(NextA, NextB, Count + 1, Acc + match(NextA, NextB)).
