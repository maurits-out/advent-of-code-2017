-module(tubes).
-export([follow_path/0]).
-define(SPACE, $\ ).

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
  [Column] = [C || C <- lists:seq(0, array:size(Row) - 1), array:get(C, Row) /= ?SPACE],
  Column.

move({Row, Column}, Direction) ->
  case Direction of
    down -> {Row + 1, Column};
    up -> {Row - 1, Column};
    left -> {Row, Column - 1};
    right -> {Row, Column + 1}
  end.

new_heading({Row, Column}, CurrentDirection, Diagram) when (CurrentDirection == down) or (CurrentDirection == up) ->
  case character({Row, Column + 1}, Diagram) of
    ?SPACE -> left;
    _ -> right
  end;
new_heading({Row, Column}, _, Diagram) ->
  case character({Row + 1, Column}, Diagram) of
    ?SPACE -> up;
    _ -> down
  end.

follow_path(Location, Direction, Diagram, Acc, Steps) ->
  case character(Location, Diagram) of
    ?SPACE ->
      {lists:reverse(Acc), Steps - 1};
    $+ ->
      NewDirection = new_heading(Location, Direction, Diagram),
      follow_path(move(Location, NewDirection), NewDirection, Diagram, Acc, Steps + 1);
    Char when Char >= $A andalso Char =< $Z ->
      follow_path(move(Location, Direction), Direction, Diagram, [Char | Acc], Steps + 1);
    _ ->
      follow_path(move(Location, Direction), Direction, Diagram, Acc, Steps + 1)
  end.

follow_path() ->
  Diagram = read_diagram(),
  {Letters, Steps} = follow_path({0, start_column(Diagram)}, down, Diagram, [], 1),
  io:format("Part 1: ~p~nPart 2: ~p~n", [Letters, Steps]).
