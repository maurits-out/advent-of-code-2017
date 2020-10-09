-module(checksum).
-export([part1/0, part2/0]).

read_lines_from_input() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

parse_line(Line) ->
  Lexemes = string:lexemes(Line, [$\t]),
  [element(1, string:list_to_integer(L)) || L <- Lexemes].

parse_lines(Lines) ->
  [parse_line(L) || L <- Lines].

calculate_checksum(Op) ->
  Lines = read_lines_from_input(),
  Lists = parse_lines(Lines),
  lists:sum([Op(L) || L <- Lists]).

evenly_divided(Numbers) ->
  Divisions = [M div N || M <- Numbers, N <- Numbers, M > N, (M rem N) =:= 0],
  lists:nth(1, Divisions).

part1() ->
  calculate_checksum(fun(L) -> lists:max(L) - lists:min(L) end).

part2() ->
  calculate_checksum(fun evenly_divided/1).
