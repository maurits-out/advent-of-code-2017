-module(spinlock).
-export([part1/0, part2/0]).
-define(ITERATIONS, 2017).
-define(PUZZLE_INPUT, 356).

spinlock_part1(Buffer, Position, 2018) ->
  lists:nth(Position + 2, Buffer);
spinlock_part1(Buffer, Position, NextValue) ->
  NextPosition = (Position + ?PUZZLE_INPUT) rem length(Buffer),
  UpdatedBuffer = lists:sublist(Buffer, NextPosition + 1) ++ [NextValue | lists:nthtail(NextPosition + 1, Buffer)],
  spinlock_part1(UpdatedBuffer, NextPosition + 1, NextValue + 1).

spinlock_part2(ValueAfter0, _, 50000001) ->
  ValueAfter0;
spinlock_part2(ValueAfter0, Position, NextValue) ->
  case (Position + ?PUZZLE_INPUT) rem NextValue of
    0 -> spinlock_part2(NextValue, 1, NextValue + 1);
    NextPosition -> spinlock_part2(ValueAfter0, NextPosition + 1, NextValue + 1)
  end.

part1() ->
  spinlock_part1([0], 0, 1).

part2() ->
  spinlock_part2(null, 0, 1).
