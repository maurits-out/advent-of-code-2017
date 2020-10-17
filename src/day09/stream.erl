-module(stream).
-export([process/0]).

process() ->
  {ok, File} = file:read_file("input.txt"),
  Stream = string:trim(unicode:characters_to_list(File)),
  process(Stream, 0, false, 0, 0).

process([], _, _, Score, GarbageCount) -> {Score, GarbageCount};
process([${ | T], Depth, false, Score, GarbageCount) -> process(T, Depth + 1, false, Score, GarbageCount);
process([$} | T], Depth, false, Score, GarbageCount) -> process(T, Depth - 1, false, Score + Depth, GarbageCount);
process([$< | T], Depth, false, Score, GarbageCount) -> process(T, Depth, true, Score, GarbageCount);
process([$> | T], Depth, true, Score, GarbageCount) -> process(T, Depth, false, Score, GarbageCount);
process([$!, _ | T], Depth, true, Score, GarbageCount) -> process(T, Depth, true, Score, GarbageCount);
process([_ | T], Depth, false, Score, GarbageCount) -> process(T, Depth, false, Score, GarbageCount);
process([_ | T], Depth, true, Score, GarbageCount) -> process(T, Depth, true, Score, GarbageCount + 1).
