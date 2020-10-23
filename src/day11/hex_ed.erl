-module(hex_ed).
-export([result/0]).

read_instructions() ->
  {ok, File} = file:read_file("input.txt"),
  Input = string:trim(unicode:characters_to_list(File)),
  string:lexemes(Input, ",").

distance(X, Y) ->
  abs(X) + abs(Y).

move(Instruction, {X, Y, MaxDistance}) ->
  {NextX, NextY} = case Instruction of
    "n" -> {X, Y + 1};
    "ne" -> {X + 1, Y + 1};
    "se" -> {X + 1, Y};
    "s" -> {X, Y - 1};
    "sw" -> {X - 1, Y - 1};
    "nw" -> {X - 1, Y}
  end,
  {NextX, NextY, max(MaxDistance, distance(NextX, NextY))}.

follow_instructions() ->
  lists:foldl(fun(I, State) -> move(I, State) end, {0, 0, 0}, read_instructions()).

result() ->
  {X, Y, MaxDistance} = follow_instructions(),
  io:format("Part 1: ~p / Part 2: ~p~n", [distance(X, Y), MaxDistance]).
