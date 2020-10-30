-module(permutation).
-export([part1/0, part2/0]).

read_moves() ->
  {ok, File} = file:read_file("input.txt"),
  Content = string:trim(unicode:characters_to_list(File)),
  string:lexemes(Content, ",").

initial_positions() ->
  "abcdefghijklmnop".

spin(X, Positions) ->
  {List1, List2} = lists:split(length(Positions) - X, Positions),
  List2 ++ List1.

exchange(_, _, _, 0, Acc) ->
  Acc;
exchange(A, B, Positions, Current, Acc) ->
  Index = case Current of
            A -> B;
            B -> A;
            _ -> Current
          end,
  exchange(A, B, Positions, Current - 1, [lists:nth(Index, Positions) | Acc]).

partner(_, _, [], Acc) ->
  lists:reverse(Acc);
partner(A, B, [H | T], Acc) ->
  Char = case H of
           A -> B;
           B -> A;
           _ -> H
         end,
  partner(A, B, T, [Char | Acc]).

move([$s | X], Positions) ->
  spin(list_to_integer(X), Positions);
move([$x | AB], Positions) ->
  [A, B] = string:lexemes(AB, "/"),
  exchange(list_to_integer(A) + 1, list_to_integer(B) + 1, Positions, length(Positions), []);
move([$p | AB], Positions) ->
  [[A], [B]] = string:lexemes(AB, "/"),
  partner(A, B, Positions, []).

dance(CurrentPositions, Moves) ->
  lists:foldl(fun(Move, Positions) -> move(Move, Positions) end, CurrentPositions, Moves).

part1() ->
  dance(initial_positions(), read_moves()).

part2() ->
  Moves = read_moves(),
  lists:foldl(fun(_, Positions) -> dance(Positions, Moves) end, initial_positions(), lists:seq(1, 1000000000)).
