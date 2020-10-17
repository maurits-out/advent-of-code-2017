-module(stream).
-export([process/0]).

process() ->
  {ok, File} = file:read_file("input.txt"),
  Stream = string:trim(unicode:characters_to_list(File)),
  process(Stream, 0, no_garbage, 0, 0).

process([], _, _, Score, GarbageCount) -> {Score, GarbageCount};
process([${ | T], Depth, no_garbage, Score, GarbageCount) -> process(T, Depth + 1, no_garbage, Score, GarbageCount);
process([$} | T], Depth, no_garbage, Score, GarbageCount) -> process(T, Depth - 1, no_garbage, Score + Depth, GarbageCount);
process([$< | T], Depth, no_garbage, Score, GarbageCount) -> process(T, Depth, garbage, Score, GarbageCount);
process([$> | T], Depth, garbage, Score, GarbageCount) -> process(T, Depth, no_garbage, Score, GarbageCount);
process([$!, _ | T], Depth, garbage, Score, GarbageCount) -> process(T, Depth, garbage, Score, GarbageCount);
process([_ | T], Depth, no_garbage, Score, GarbageCount) -> process(T, Depth, no_garbage, Score, GarbageCount);
process([_ | T], Depth, garbage, Score, GarbageCount) -> process(T, Depth, garbage, Score, GarbageCount + 1).
