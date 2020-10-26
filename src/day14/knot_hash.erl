-module(knot_hash).
-export([knot/1]).

-define(ADDITIONAL_LENGTHS, [17, 31, 73, 47, 23]).
-define(XOR_BLOCK_LENGTH, 16).
-define(HASH_ROUNDS, 64).

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

sparse_hash(_, List, ?HASH_ROUNDS, TotalSteps) ->
  rotate_back(List, TotalSteps);
sparse_hash(Lengths, List, Round, TotalSteps) ->
  {NewList, Steps} = hash_round(Lengths, List, Round * length(Lengths), 0),
  sparse_hash(Lengths, NewList, Round + 1, TotalSteps + Steps).

xor_list(List) ->
  lists:foldl(fun(N, Acc) -> N bxor Acc end, 0, List).

dense_hash([], Result) ->
  lists:reverse(Result);
dense_hash(List, Result) ->
  {Block, Rest} = lists:split(?XOR_BLOCK_LENGTH, List),
  dense_hash(Rest, [xor_list(Block) | Result]).

convert_to_hex(DenseHash) ->
  lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- DenseHash]).

knot(Input) ->
  Lengths = Input ++ ?ADDITIONAL_LENGTHS,
  SparseHash = sparse_hash(Lengths, initial_list(), 0, 0),
  convert_to_hex(dense_hash(SparseHash, [])).
