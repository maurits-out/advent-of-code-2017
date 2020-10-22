-module(knot_hash).
-export([part1/0, part2/0]).
-include_lib("eunit/include/eunit.hrl").

-define(ADDITIONAL_LENGTHS, [17, 31, 73, 47, 23]).
-define(XOR_BLOCK_LENGTH, 16).

puzzle_input() ->
  "76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229".

initial_list() ->
  lists:seq(0, 255).

reverse_section(List, SectionLength) ->
  {L1, L2} = lists:split(SectionLength, List),
  lists:reverse(L1) ++ L2.

rotate(List, Steps) ->
  {L1, L2} = lists:split(Steps, List),
  L2 ++ L1.

rotate_back(List, TotalSteps) ->
  Steps = length(List) - TotalSteps rem length(List),
  rotate(List, Steps).

hash_round([], List, _, TotalSteps) ->
  {List, TotalSteps};
hash_round([Length | T], List, SkipSize, TotalSteps) ->
  Steps = (Length + SkipSize) rem length(List),
  NewList = rotate(reverse_section(List, Length), Steps),
  hash_round(T, NewList, SkipSize + 1, TotalSteps + Steps).

sparse_hash(_, List, 64, TotalSteps) ->
  rotate_back(List, TotalSteps);
sparse_hash(Lengths, List, Round, TotalSteps) ->
  {NewList, Steps} = hash_round(Lengths, List, Round * length(Lengths), 0),
  sparse_hash(Lengths, NewList, Round + 1, TotalSteps + Steps).

xor_list(List) ->
  lists:foldl(fun (N, Acc) -> N bxor Acc end, 0, List).

dense_hash([], Result) ->
  lists:reverse(Result);
dense_hash(List, Result) ->
  {Block, Rest} = lists:split(?XOR_BLOCK_LENGTH, List),
  dense_hash(Rest, [xor_list(Block) | Result]).
dense_hash(List) ->
  dense_hash(List, []).

knot(Input) ->
  Lengths = Input ++ ?ADDITIONAL_LENGTHS,
  SparseHash = sparse_hash(Lengths, initial_list(), 0, 0),
  DenseHash = dense_hash(SparseHash),
  lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- DenseHash]).

part1() ->
  Lengths = [list_to_integer(S) || S <- string:lexemes(puzzle_input(), [$,])],
  {List, TotalSteps} = hash_round(Lengths, initial_list(), 0, 0),
  [N, M | _] = rotate_back(List, TotalSteps),
  N * M.

part2() ->
  knot(puzzle_input()).

%% Tests

hash_round_test() ->
  ?assertEqual({[0, 3, 4, 2, 1], 13}, hash_round([3, 4, 1, 5], [0, 1, 2, 3, 4], 1, 0)).

xor_list_test() ->
  ?assertEqual(64, xor_list([65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22])).

knot_test() ->
  ?assertEqual("a2582a3a0e66e6e86e3812dcb672a272", knot("")),
  ?assertEqual("33efeb34ea91902bb2f59c9920caa6cd", knot("AoC 2017")),
  ?assertEqual("3efbe78a8d82f29979031a4aa0b16a9d", knot("1,2,3")),
  ?assertEqual("63960835bcdc130f0b66d7ff4f6a5a8e", knot("1,2,4")).
