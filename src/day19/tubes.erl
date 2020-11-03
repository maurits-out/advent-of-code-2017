-module(tubes).
-export([part1/0]).

to_array(Bin) ->
  array:from_list(binary_to_list(Bin)).

read_diagram() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  Lines = [to_array(B) || B <- Bins, byte_size(B) > 0],
  array:from_list(Lines).

character({Row, Column}, Diagram) ->
  array:get(Column, array:get(Row, Diagram)).

start_column(Diagram) ->
  Row = array:get(0, Diagram),
  [Column] = [C || C <- lists:seq(0, array:size(Row) - 1), array:get(C, Row) /= $\ ],
  Column.

move({Row, Column}, Direction) ->
  case Direction of
    down -> {Row + 1, Column};
    up -> {Row - 1, Column};
    left -> {Row, Column - 1};
    right -> {Row, Column + 1}
  end.

follow_path(Location, Direction, Diagram, Acc) ->
  case character(Location, Diagram) of
    $+ ->
      Location;
    _ ->
      follow_path(move(Location, Direction), Direction, Diagram, Acc)
  end.

part1() ->
  Diagram = read_diagram(),
  follow_path({0, start_column(Diagram)}, down, Diagram, []).



