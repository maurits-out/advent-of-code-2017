-module(reallocation).
-export([part1/0, part2/0]).

puzzle_input() -> array:from_list([11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]).

max(CurrentMaxIndex, Index, Banks) ->
  case array:get(CurrentMaxIndex, Banks) >= array:get(Index, Banks) of
    true -> CurrentMaxIndex;
    false -> Index
  end.

bank_with_most_blocks(Banks, Index, CurrentMaxIndex) ->
  case Index == array:size(Banks) of
    true ->
      CurrentMaxIndex;
    false ->
      bank_with_most_blocks(Banks, Index + 1, max(CurrentMaxIndex, Index, Banks))
  end.

next_bank_index(Index, Banks) ->
  (Index + 1) rem array:size(Banks).

reallocate_blocks(Banks, _, 0) -> Banks;
reallocate_blocks(Banks, Index, Remaining) ->
  UpdatedBanks = array:set(Index, array:get(Index, Banks) + 1, Banks),
  NextBank = next_bank_index(Index, Banks),
  reallocate_blocks(UpdatedBanks, NextBank, Remaining - 1).

reallocate_blocks(Banks) ->
  Index = bank_with_most_blocks(Banks, 1, 0),
  Removed = array:set(Index, 0, Banks),
  NextBank = next_bank_index(Index, Banks),
  Remaining = array:get(Index, Banks),
  reallocate_blocks(Removed, NextBank, Remaining).

reallocate(Banks, Produced, Cycle) ->
  case sets:is_element(Banks, Produced) of
    true ->
      {Cycle, Banks};
    false ->
      reallocate(reallocate_blocks(Banks), sets:add_element(Banks, Produced), Cycle + 1)
  end.

part1() ->
  {Cycles, _} = reallocate(puzzle_input(), sets:new(), 0),
  Cycles.

part2() ->
  {_, Banks} = reallocate(puzzle_input(), sets:new(), 0),
  {Cycles, _} = reallocate(Banks, sets:new(), 0),
  Cycles.
