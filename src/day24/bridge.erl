-module(bridge).
-export([part1/0]).

read_input() ->
  {ok, Bin} = file:read_file("src/day24/input.txt"),
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

strength_of_strongest_bridge(CurrentPin, Remaining, Bridge) ->
  case find_matching_components(CurrentPin, Remaining) of
    [] ->
      strength(Bridge);
    Matching ->
      lists:max([extend_bridge(CurrentPin, C, Remaining, Bridge) || C <- Matching])
  end.

extend_bridge(CurrentPin, NewComponent, Remaining, Bridge) ->
  NextPin = next_pin(CurrentPin, NewComponent),
  NewRemaining = sets:del_element(NewComponent, Remaining),
  strength_of_strongest_bridge(NextPin, NewRemaining, [NewComponent | Bridge]).

part1() ->
  Strength = strength_of_strongest_bridge(0, read_components(), []),
  io:format("Strength of the strongest bridge: ~p~n", [Strength]).
