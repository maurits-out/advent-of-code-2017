-module(firewall).
-export([part1/0, part2/0]).

read_lines_from_input() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

parse_line(Line) ->
  [DepthString, RangeString] = string:lexemes(Line, ": "),
  {list_to_integer(DepthString), list_to_integer(RangeString)}.

read_firewall() ->
  [parse_line(L) || L <- read_lines_from_input()].

scanner_on_top(Time, Range) ->
  Time rem (2 * (Range - 1)) == 0.

layer_severity(Depth, Range) ->
  case scanner_on_top(Depth, Range) of
    true  -> Depth * Range;
    false -> 0
  end.

total_severity(Firewall) ->
  lists:sum([layer_severity(Depth, Range) || {Depth, Range} <- Firewall]).

caught(Delay, Firewall) ->
  lists:any(fun({Depth, Range}) -> scanner_on_top(Delay + Depth, Range) end, Firewall).

smallest_delay(Delay, Firewall) ->
  case caught(Delay, Firewall) of
    true  -> smallest_delay(Delay + 1, Firewall);
    false -> Delay
  end.

part1() -> total_severity(read_firewall()).

part2() -> smallest_delay(0, read_firewall()).
