-module(stream).
-export([part1/0, part2/0]).

read_stream() ->
  {ok, File} = file:read_file("input.txt"),
  string:trim(unicode:characters_to_list(File)).

process([], _, _, Score, GarbageCount) -> {Score, GarbageCount};
process([${ | T], Depth, false, Score, GarbageCount) -> process(T, Depth + 1, false, Score, GarbageCount);
process([$} | T], Depth, false, Score, GarbageCount) -> process(T, Depth - 1, false, Score + Depth, GarbageCount);
process([$< | T], Depth, false, Score, GarbageCount) -> process(T, Depth, true, Score, GarbageCount);
process([$> | T], Depth, true, Score, GarbageCount) -> process(T, Depth, false, Score, GarbageCount);
process([$!, _ | T], Depth, true, Score, GarbageCount) -> process(T, Depth, true, Score, GarbageCount);
process([_ | T], Depth, false, Score, GarbageCount) -> process(T, Depth, false, Score, GarbageCount);
process([_ | T], Depth, true, Score, GarbageCount) -> process(T, Depth, true, Score, GarbageCount + 1).

part1() ->
  {TotalScore, _} = process(read_stream(), 0, false, 0, 0),
  TotalScore.

part2() ->
  {_, GarbageCount} = process(read_stream(), 0, false, 0, 0),
  GarbageCount.
