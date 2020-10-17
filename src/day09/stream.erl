-module(stream).
-export([part1/0]).

read_stream() ->
  {ok, File} = file:read_file("input.txt"),
  string:trim(unicode:characters_to_list(File)).

total_score([], _, _, Score) -> Score;
total_score([${ | T], Depth, false, Score) -> total_score(T, Depth + 1, false, Score);
total_score([$} | T], Depth, false, Score) -> total_score(T, Depth - 1, false, Score + Depth);
total_score([$< | T], Depth, false, Score) -> total_score(T, Depth, true, Score);
total_score([$> | T], Depth, true, Score) -> total_score(T, Depth, false, Score);
total_score([$!, _ | T], Depth, true, Score) -> total_score(T, Depth, true, Score);
total_score([_ | T], Depth, Garbage, Score) -> total_score(T, Depth, Garbage, Score).

part1() ->
  total_score(read_stream(), 0, false, 0).
