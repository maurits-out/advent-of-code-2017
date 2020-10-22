-module(knot_hash).
-export([part1/0, part2/0]).
-include_lib("eunit/include/eunit.hrl").

puzzle_input() ->
  "76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229".

initial_list() ->
  lists:seq(0, 255).

extract_subsections(Position, Length, List) ->
  case Position + Length - length(List) - 1 of
    Diff when Diff > 0 ->
      {lists:sublist(List, Position, length(List) - Position + 1), lists:sublist(List, Diff)};
    _ ->
      {lists:sublist(List, Position, Length), []}
  end.

reverse_subsections({S1, S2}) ->
  lists:split(length(S1), lists:reverse(S1 ++ S2)).

replace_subsections(Position, {R1, []}, List) ->
  FirstPart = lists:sublist(List, Position - 1) ++ R1,
  SecondPart = lists:sublist(List, length(FirstPart) + 1, length(List) - length(FirstPart)),
  FirstPart ++ SecondPart;
replace_subsections(_, {R1, R2}, List) ->
  R2 ++ lists:sublist(List, length(R2) + 1, length(List) - length(R1) - length(R2)) ++ R1.

reverse_section(Position, Length, List) ->
  {S1, S2} = extract_subsections(Position, Length, List),
  {R1, R2} = reverse_subsections({S1, S2}),
  replace_subsections(Position, {R1, R2}, List).

hash_round([], List, Position, SkipSize) ->
  {List, Position, SkipSize};
hash_round([Length | T], List, Position, SkipSize) ->
  NewList = reverse_section(Position, Length, List),
  NewPosition = (Position + Length + SkipSize - 1) rem length(List) + 1,
  hash_round(T, NewList, NewPosition, SkipSize + 1).

sparse_hash(_, List, _, _, 64) ->
  List;
sparse_hash(Lengths, List, Position, SkipSize, Round) ->
  {NewList, NewPosition, NewSkipSize} = hash_round(Lengths, List, Position, SkipSize),
  sparse_hash(Lengths, NewList, NewPosition, NewSkipSize, Round + 1).

xor_list([], Result) ->
  Result;
xor_list([H | T], Result) ->
  xor_list(T, H bxor Result).
xor_list(L) ->
  xor_list(L, 0).

dense_hash([], Result) ->
  lists:reverse(Result);
dense_hash(List, Result) ->
  {Block, Rest} = lists:split(16, List),
  dense_hash(Rest, [xor_list(Block) | Result]).
dense_hash(List) ->
  dense_hash(List, []).

knot_hash(Input) ->
  Lengths = Input ++ [17, 31, 73, 47, 23],
  SparseHash = sparse_hash(Lengths, initial_list(), 1, 0, 0),
  DenseHash = dense_hash(SparseHash),
  lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- DenseHash]).

part1() ->
  Lengths = [list_to_integer(S) || S <- string:lexemes(puzzle_input(), [$,])],
  {List, _, _} = hash_round(Lengths, initial_list(), 1, 0),
  [N, M | _] = List,
  N * M.

part2() ->
  knot_hash(puzzle_input()).

%% Tests

extract_subsections_test() ->
  ?assertEqual({[2, 3, 4], []}, extract_subsections(2, 3, [1, 2, 3, 4, 5])),
  ?assertEqual({[3, 4, 5], [1, 2]}, extract_subsections(3, 5, [1, 2, 3, 4, 5])),
  ?assertEqual({[5], [1, 2, 3]}, extract_subsections(5, 4, [1, 2, 3, 4, 5])),
  ?assertEqual({[1, 2, 3, 4, 5], []}, extract_subsections(1, 5, [1, 2, 3, 4, 5])).

reverse_subsections_test() ->
  ?assertEqual({[4, 3, 2], []}, reverse_subsections({[2, 3, 4], []})),
  ?assertEqual({[2, 1, 5], [4, 3]}, reverse_subsections({[3, 4, 5], [1, 2]})).

replace_subsections_test() ->
  ?assertEqual([1, 2, 4, 3, 5], replace_subsections(3, {[4, 3], []}, [1, 2, 3, 4, 5])),
  ?assertEqual([5, 4, 3, 2, 1], replace_subsections(1, {[5, 4, 3, 2, 1], []}, [1, 2, 3, 4, 5])),
  ?assertEqual([5, 4, 3, 2, 1], replace_subsections(3, {[3, 2, 1], [5, 4]}, [1, 2, 3, 4, 5])),
  ?assertEqual([4, 5, 3, 1, 2], replace_subsections(4, {[1, 2], [4, 5]}, [1, 2, 3, 4, 5])).

calculate_hash_test() ->
  ?assertEqual({[3, 4, 2, 1, 0], 5, 4}, hash_round([3, 4, 1, 5], [0, 1, 2, 3, 4], 1, 0)).

xor_list_test() ->
  ?assertEqual(64, xor_list([65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22])).

knot_hash_test() ->
  ?assertEqual("a2582a3a0e66e6e86e3812dcb672a272", knot_hash("")),
  ?assertEqual("33efeb34ea91902bb2f59c9920caa6cd", knot_hash("AoC 2017")),
  ?assertEqual("3efbe78a8d82f29979031a4aa0b16a9d", knot_hash("1,2,3")),
  ?assertEqual("63960835bcdc130f0b66d7ff4f6a5a8e", knot_hash("1,2,4")).