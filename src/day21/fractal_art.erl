-module(fractal_art).
-export([part1/0, part2/0]).
-define(INITIAL_GRID, [".#.", "..#", "###"]).
-include_lib("eunit/include/eunit.hrl").

read_input() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

parse_pattern(Pattern) ->
  string:lexemes(Pattern, "/").

parse_line(Line) ->
  [Left, Right] = string:lexemes(Line, " => "),
  {parse_pattern(Left), parse_pattern(Right)}.

read_rules() ->
  [parse_line(L) || L <- read_input()].

flip(Pattern) ->
  [lists:reverse(R) || R <- Pattern].

look_up(Pattern, Row, Column) ->
  lists:nth(Column, lists:nth(Row, Pattern)).

rotate_row(Pattern, Row) ->
  [look_up(Pattern, length(Pattern) - C + 1, Row) || C <- lists:seq(1, length(Pattern))].

rotate(Pattern) ->
  [rotate_row(Pattern, Row) || Row <- lists:seq(1, length(Pattern))].

rotations(Pattern) ->
  lists:foldl(fun(_, [H | _] = A) -> [rotate(H) | A] end, [Pattern], lists:seq(1, 3)).

append_flip_patterns(Patterns) ->
  Patterns ++ [flip(P) || P <- Patterns].

extend_rule({Left, Right}) ->
  [{P, Right} || P <- append_flip_patterns(rotations(Left))].

extend_rules([], Acc) ->
  Acc;
extend_rules([Rule | Others], Acc) ->
  extend_rules(Others, extend_rule(Rule) ++ Acc).

calculate_square_size(Grid) ->
  case length(Grid) rem 2 of
    0 -> 2;
    _ -> 3
  end.

extract_square(Grid, RowNumber, ColumnNumber, SquareSize) ->
  [lists:sublist(lists:nth(R, Grid), ColumnNumber, SquareSize) || R <- lists:seq(RowNumber, RowNumber + SquareSize - 1)].

split_rows_to_squares(Grid, Row, SquareSize) ->
  [extract_square(Grid, Row, Column, SquareSize) || Column <- lists:seq(1, length(Grid), SquareSize)].

split_to_squares(Grid) ->
  SquareSize = calculate_square_size(Grid),
  [split_rows_to_squares(Grid, Row, SquareSize) || Row <- lists:seq(1, length(Grid), SquareSize)].

apply_rules_to_row(SquareRow, Rules) ->
  [maps:get(Square, Rules) || Square <- SquareRow].

apply_rules(Squares, Rules) ->
  [apply_rules_to_row(SquareRow, Rules) || SquareRow <- Squares].

join_rows_of_squares(SquaresRow, RowNumber) ->
  lists:flatten([lists:nth(RowNumber, Square) || Square <- SquaresRow]).

join_squares_row(SquaresRow) ->
  [join_rows_of_squares(SquaresRow, R) || R <- lists:seq(1, length(lists:nth(1, SquaresRow)))].

join_squares_to_grid(Squares) ->
  lists:concat([join_squares_row(SquaresRow) || SquaresRow <- Squares]).

update(Grid, Rules) ->
  Squares = split_to_squares(Grid),
  join_squares_to_grid(apply_rules(Squares, Rules)).

count_pixels(Grid) ->
  length(lists:filter(fun(P) -> P == $# end, lists:flatten(Grid))).

iterate(Times) ->
  Rules = maps:from_list(extend_rules(read_rules(), [])),
  Grid = lists:foldl(fun(_, G) -> update(G, Rules) end, ?INITIAL_GRID, lists:seq(1, Times)),
  count_pixels(Grid).

part1() ->
  iterate(5).

part2() ->
  iterate(18).

%% Tests

parse_pattern_test() ->
  ?assertEqual([".#", "#."], parse_pattern(".#/#.")),
  ?assertEqual(["#.#", "...", "#.."], parse_pattern("#.#/.../#..")).

parse_line_test() ->
  ?assertEqual({["##", ".#"], [".##", "#..", "##."]}, parse_line("##/.# => .##/#../##.")).

flip_test() ->
  ?assertEqual(["..#", "#..", ".#."], flip(["#..", "..#", ".#."])).

look_up_test() ->
  ?assertEqual($1, look_up(["12", "34"], 1, 1)),
  ?assertEqual($4, look_up(["12", "34"], 2, 2)).

rotate_test() ->
  ?assertEqual([".#", "#."], rotate(["#.", ".#"])),
  ?assertEqual(["#..", "#.#", "##."], rotate([".#.", "..#", "###"])).

rotations_test() ->
  ?assertEqual([[".##","#.#","..#"],
                ["###","#..",".#."],
                ["#..","#.#","##."],
                [".#.","..#","###"]],
               rotations([".#.", "..#", "###"])).

append_flip_patterns_test() ->
  ?assertEqual([[".##","#.#","..#"],
                ["###","#..",".#."],
                ["##.","#.#","#.."],
                ["###","..#",".#."]],
               append_flip_patterns([[".##","#.#","..#"],
                                     ["###","#..",".#."]])).

extend_rule_test() ->
  ?assertEqual([{["#..","##.","..#"],["###.","#..#",".##.","###."]},
                {["..#",".#.","##."],["###.","#..#",".##.","###."]},
                {["#..",".##","..#"],["###.","#..#",".##.","###."]},
                {[".##",".#.","#.."],["###.","#..#",".##.","###."]},
                {["..#",".##","#.."],["###.","#..#",".##.","###."]},
                {["#..",".#.",".##"],["###.","#..#",".##.","###."]},
                {["..#","##.","#.."],["###.","#..#",".##.","###."]},
                {["##.",".#.","..#"],["###.","#..#",".##.","###."]}],
               extend_rule({[".##", ".#.", "#.."], ["###.", "#..#", ".##.", "###."]})).
