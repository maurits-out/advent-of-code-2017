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
  maps:from_list([parse_line(L) || L <- read_lines_from_input()]).

normalize_time(Time, Range) ->
  Time rem (2 * (Range - 1)).

scanner_position(Time, Range) ->
  case normalize_time(Time, Range) of
    T when T < Range -> T;
    T -> 2 * (Range - 1) - T
  end.

layer_severity(Delay, Depth, Range) ->
  case scanner_position(Delay + Depth, Range) of
    0 -> Depth * Range;
    _ -> 0
  end.

total_severity(Firewall) ->
  lists:sum([layer_severity(0, Depth, Range) || {Depth, Range} <- maps:to_list(Firewall)]).

caught(Delay, Firewall) ->
  Positions = [scanner_position(Delay + Depth, Range) || {Depth, Range} <- maps:to_list(Firewall)],
  lists:any(fun(P) -> P == 0 end, Positions).

smallest_delay(Delay, Firewall) ->
  case caught(Delay, Firewall) of
    false -> Delay;
    true -> smallest_delay(Delay + 1, Firewall)
  end.

part1() -> total_severity(read_firewall()).

part2() -> smallest_delay(0, read_firewall()).
