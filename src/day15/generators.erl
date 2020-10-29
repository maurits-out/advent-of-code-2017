-module(generators).
-export([part1/0, part2/0]).

part1() ->
  count_matching_pairs(create_generator(16807), create_generator(48271), 40000000).

part2() ->
  count_matching_pairs(create_generator(16807, 4), create_generator(48271, 8), 5000000).

puzzle_input() ->
  {634, 301}.

create_generator(Factor) ->
  fun(Value) -> (Value * Factor) rem 2147483647 end.

create_generator(Factor, MultipleOf) ->
  GeneratorPart1 = create_generator(Factor),
  fun Generator(Value) ->
    NextValue = GeneratorPart1(Value),
    case NextValue rem MultipleOf of
      0 -> NextValue;
      _ -> Generator(NextValue)
    end
  end.

match(A, B) ->
  case (A band 16#ffff) == (B band 16#ffff) of
    true -> 1;
    false -> 0
  end.

count_matching_pairs(GeneratorA, GeneratorB, Iterations) ->
  {StartA, StartB} = puzzle_input(),
  count_matching_pairs(StartA, StartB, GeneratorA, GeneratorB, Iterations, 0).

count_matching_pairs(_, _, _, _, 0, Acc) ->
  Acc;
count_matching_pairs(A, B, GeneratorA, GeneratorB, Count, Acc) ->
  NextA = GeneratorA(A),
  NextB = GeneratorB(B),
  count_matching_pairs(NextA, NextB, GeneratorA, GeneratorB, Count - 1, Acc + match(NextA, NextB)).
