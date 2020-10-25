-module(firewall).
-export([part1/0]).

read_lines_from_input() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

parse_line(Line) ->
  [DepthString, RangeString] = string:lexemes(Line, ": "),
  {list_to_integer(DepthString), list_to_integer(RangeString)}.

read_firewall() ->
  maps:from_list([parse_line(L) || L <- read_lines_from_input()]).

normalize_time(Time, Range) ->
  Time rem (2 * Range - 2).

scanner_position(Time, Range) ->
  case normalize_time(Time, Range) of
    T when T < Range -> T;
    T -> (2 * Range - 2)  - T
  end.

layer_severity(Depth, Range) ->
  case scanner_position(Depth, Range) of
    0 -> Depth * Range;
    _ -> 0
  end.

total_severity(Firewall) ->
  lists:sum([layer_severity(Depth, Range) || {Depth, Range} <- maps:to_list(Firewall)]).

part1() ->
  Firewall = read_firewall(),
  total_severity(Firewall).
