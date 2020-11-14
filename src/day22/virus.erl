-module(virus).
-export([count_infected/0]).

read_input() ->
  {ok, Bin} = file:read_file("src/day22/input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

initial_position(Grid) ->
  M = length(Grid) div 2,
  {M, M}.

infected_in_row([], _, _, InfectedLocations) ->
  InfectedLocations;
infected_in_row([Node | T], RowNumber, ColumnNumber, InfectedLocations) ->
  Updated = case Node of
              $# -> [{RowNumber, ColumnNumber} | InfectedLocations];
              _ -> InfectedLocations
            end,
  infected_in_row(T, RowNumber, ColumnNumber + 1, Updated).

grid_as_map([], _, InfectedLocations) ->
  maps:from_list([{P, infected} || P <- InfectedLocations]);
grid_as_map([Row | Remaining], RowNumber, InfectedLocations) ->
  grid_as_map(Remaining, RowNumber + 1, InfectedLocations ++ infected_in_row(Row, RowNumber, 0, [])).

turn_right(Heading) ->
  case Heading of
    {-1, 0} -> {0, 1};
    {0, 1} -> {1, 0};
    {1, 0} -> {0, -1};
    {0, -1} -> {-1, 0}
  end.

turn_left(Heading) ->
  case Heading of
    {-1, 0} -> {0, -1};
    {0, -1} -> {1, 0};
    {1, 0} -> {0, 1};
    {0, 1} -> {-1, 0}
  end.

move({Row, Column}, {DeltaRow, DeltaColumn}) ->
  {Row + DeltaRow, Column + DeltaColumn}.

burst_part1(_Grid, _Heading, _Location, InfectedCount, 10000) ->
  InfectedCount;
burst_part1(Grid, Heading, Location, InfectedCount, Activities) ->
  case maps:get(Location, Grid, clean) of
    infected ->
      NewHeading = turn_right(Heading),
      UpdatedGrid = maps:remove(Location, Grid),
      burst_part1(UpdatedGrid, NewHeading, move(Location, NewHeading), InfectedCount, Activities + 1);
    clean ->
      NewHeading = turn_left(Heading),
      UpdatedGrid = maps:put(Location, infected, Grid),
      burst_part1(UpdatedGrid, NewHeading, move(Location, NewHeading), InfectedCount + 1, Activities + 1)
  end.

burst_part2(_Grid, _Heading, _Location, InfectedCount, 10000000) ->
  InfectedCount;
burst_part2(Grid, Heading, Location, InfectedCount, Activities) ->
  case maps:get(Location, Grid, clean) of
    clean ->
      NewHeading = turn_left(Heading),
      UpdatedGrid = maps:put(Location, weakened, Grid),
      burst_part2(UpdatedGrid, NewHeading, move(Location, NewHeading), InfectedCount, Activities + 1);
    weakened ->
      UpdatedGrid = maps:put(Location, infected, Grid),
      burst_part2(UpdatedGrid, Heading, move(Location, Heading), InfectedCount + 1, Activities + 1);
    infected ->
      NewHeading = turn_right(Heading),
      UpdatedGrid = maps:put(Location, flagged, Grid),
      burst_part2(UpdatedGrid, NewHeading, move(Location, NewHeading), InfectedCount, Activities + 1);
    flagged ->
      NewHeading = turn_right(turn_right(Heading)),
      UpdatedGrid = maps:remove(Location, Grid),
      burst_part2(UpdatedGrid, NewHeading, move(Location, NewHeading), InfectedCount, Activities + 1)
  end.

part1(Grid, InitialPosition) ->
  burst_part1(Grid, {-1, 0}, InitialPosition, 0, 0).

part2(Grid, InitialPosition) ->
  burst_part2(Grid, {-1, 0}, InitialPosition, 0, 0).

count_infected() ->
  Input = read_input(),
  Grid = grid_as_map(Input, 0, []),
  InitialPosition = initial_position(Input),
  io:format("Number of nodes infected (part 1): ~p~n", [part1(Grid, InitialPosition)]),
  io:format("Number of nodes infected (part 2): ~p~n", [part2(Grid, InitialPosition)]).
