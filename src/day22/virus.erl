-module(virus).
-export([part1/0]).

read_input() ->
  {ok, Bin} = file:read_file("src/day22/input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

initial_position(Grid) ->
  M = length(Grid) div 2,
  {M, M}.

grid_row_as_set([], _, _, Acc) ->
  Acc;
grid_row_as_set([Node | T], RowNumber, ColumnNumber, Acc) ->
  UpdatedAcc = case Node of
                 $# -> sets:add_element({RowNumber, ColumnNumber}, Acc);
                 _ -> Acc
               end,
  grid_row_as_set(T, RowNumber, ColumnNumber + 1, UpdatedAcc).

grid_as_set([], _, Acc) ->
  Acc;
grid_as_set([Row | Remaining], RowNumber, Acc) ->
  grid_as_set(Remaining, RowNumber + 1, sets:union(grid_row_as_set(Row, RowNumber, 0, sets:new()), Acc)).

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

burst(_Grid, _Heading, _Location, InfectedCount, 0) ->
  InfectedCount;
burst(Grid, Heading, Location, InfectedCount, Activities) ->
  case sets:is_element(Location, Grid) of
    true ->
      NewHeading = turn_right(Heading),
      UpdatedGrid = sets:del_element(Location, Grid),
      burst(UpdatedGrid, NewHeading, move(Location, NewHeading), InfectedCount, Activities - 1);
    false ->
      NewHeading = turn_left(Heading),
      UpdatedGrid = sets:add_element(Location, Grid),
      burst(UpdatedGrid, NewHeading, move(Location, NewHeading), InfectedCount + 1, Activities - 1)
  end.

part1() ->
  Input = read_input(),
  Grid = grid_as_set(Input, 0, sets:new()),
  InfectedCount = burst(Grid, {-1, 0}, initial_position(Input), 0, 10000),
  io:format("After 10000 bursts of activity ~p nodes became infected (part 1).~n", [InfectedCount]).
