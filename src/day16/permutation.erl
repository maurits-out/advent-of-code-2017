-module(permutation).
-export([part1/0, part2/0]).
-define(INITIAL_POSITIONS, "abcdefghijklmnop").
-define(NUM_DANCES, 1000000000).

read_moves() ->
  {ok, File} = file:read_file("input.txt"),
  Content = string:trim(unicode:characters_to_list(File)),
  string:lexemes(Content, ",").

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

move_forward(Iteration) ->
  ?NUM_DANCES - (?NUM_DANCES rem (Iteration + 1)).

dance_as_a_whole(Positions, _, ?NUM_DANCES) ->
  Positions;
dance_as_a_whole(Positions, Moves, Iteration) ->
  NextPositions = dance(Positions, Moves),
  case NextPositions of
    ?INITIAL_POSITIONS ->
      dance_as_a_whole(NextPositions, Moves, move_forward(Iteration));
    _ ->
      dance_as_a_whole(NextPositions, Moves, Iteration + 1)
  end.

part1() ->
  dance(?INITIAL_POSITIONS, read_moves()).

part2() ->
  dance_as_a_whole(?INITIAL_POSITIONS, read_moves(), 0).
