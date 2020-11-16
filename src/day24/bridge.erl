-module(bridge).
-export([part1/0, part2/0]).

read_input() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

parse_line(Line) ->
  [Left, Right] = string:lexemes(Line, "/"),
  {list_to_integer(Left), list_to_integer(Right)}.

read_components() ->
  sets:from_list([parse_line(L) || L <- read_input()]).

strength(Bridge) ->
  lists:sum([L + R || {L, R} <- Bridge]).

find_matching_components(Pin, Components) ->
  [C || {L, R} = C <- sets:to_list(Components), L == Pin orelse R == Pin].

next_pin(CurrentPin, {CurrentPin, OtherPin}) -> OtherPin;
next_pin(CurrentPin, {OtherPin, CurrentPin}) -> OtherPin.

find_bridge(Select, CurrentPin, Remaining, Bridge) ->
  case find_matching_components(CurrentPin, Remaining) of
    [] ->
      Bridge;
    Matching ->
      Select([extend_bridge(Select, CurrentPin, C, Remaining, Bridge) || C <- Matching])
  end.

extend_bridge(Select, CurrentPin, NewComponent, Remaining, Bridge) ->
  NextPin = next_pin(CurrentPin, NewComponent),
  NewRemaining = sets:del_element(NewComponent, Remaining),
  find_bridge(Select, NextPin, NewRemaining, [NewComponent | Bridge]).

compare_strength(Bridge1, Bridge2) ->
  strength(Bridge1) >= strength(Bridge2).

select_strongest(Bridges) ->
  Sorted = lists:sort(fun compare_strength/2, Bridges),
  lists:nth(1, Sorted).

compare_length(Bridge1, Bridge2) ->
  length(Bridge1) > length(Bridge2) orelse
    (length(Bridge1) == length(Bridge2) andalso strength(Bridge1) >= strength(Bridge2)).

select_longest(Bridges) ->
  Sorted = lists:sort(fun compare_length/2, Bridges),
  lists:nth(1, Sorted).

part1() ->
  Strength = strength(find_bridge(fun select_strongest/1, 0, read_components(), [])),
  io:format("Strength of the strongest bridge: ~p~n", [Strength]).

part2() ->
  Strength = strength(find_bridge(fun select_longest/1, 0, read_components(), [])),
  io:format("Strength of the longest bridge: ~p~n", [Strength]).
