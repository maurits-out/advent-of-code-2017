-module(fractal_art).
-export([part1/0]).

read_input() ->
  {ok, Bin} = file:read_file("src/day21/input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

parse_pattern(Pattern) ->
  string:lexemes(Pattern, "/").

parse_line(Line) ->
  [Left, Right] = string:lexemes(Line, " => "),
  {parse_pattern(Left), parse_pattern(Right)}.

read_rules() ->
  maps:from_list([parse_line(L) || L <- read_input()]).

flip_vertical(Pattern) ->
  lists:reverse(Pattern).

flip_horizontal(Pattern) ->
  [lists:reverse(R) || R <- Pattern].

part1() ->
  io:format("~p~n", [read_rules()]).