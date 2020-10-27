-module(defragmentation).
-export([part1/0, part2/0]).
-define(PUZZLE_INPUT, "oundnydw").
-define(GRID_SIZE, 128).

create_bit_mapping() ->
  #{
    $0 => "0000",
    $1 => "0001",
    $2 => "0010",
    $3 => "0011",
    $4 => "0100",
    $5 => "0101",
    $6 => "0110",
    $7 => "0111",
    $8 => "1000",
    $9 => "1001",
    $a => "1010",
    $b => "1011",
    $c => "1100",
    $d => "1101",
    $e => "1110",
    $f => "1111"
  }.

create_grid_row(Sequence, BitMapping) ->
  HashInput = ?PUZZLE_INPUT ++ "-" ++ integer_to_list(Sequence),
  Hash = knot_hash:knot(HashInput),
  lists:flatten([maps:get(Char, BitMapping) || Char <- Hash]).

create_grid() ->
  BitMapping = create_bit_mapping(),
  [create_grid_row(S, BitMapping) || S <- lists:seq(0, ?GRID_SIZE - 1)].

grid_row_to_coordinates([], _, _, Acc) ->
  Acc;
grid_row_to_coordinates([$1 | T], RowNum, ColNum, Acc) ->
  grid_row_to_coordinates(T, RowNum, ColNum + 1, [{RowNum, ColNum} | Acc]);
grid_row_to_coordinates([$0 | T], RowNumber, ColumnNumber, Acc) ->
  grid_row_to_coordinates(T, RowNumber, ColumnNumber + 1, Acc).

grid_to_coordinates([], _, Acc) ->
  Acc;
grid_to_coordinates([Row | Rest], RowNumber, Acc) ->
  NewAcc = grid_row_to_coordinates(Row, RowNumber, 0, []) ++ Acc,
  grid_to_coordinates(Rest, RowNumber + 1, NewAcc).

adjacent_new_used_squares({Row, Column}, UsedSquares, Visited) ->
  Adjacent = [{Row - 1, Column}, {Row, Column + 1}, {Row + 1, Column}, {Row, Column - 1}],
  [Square || Square <- Adjacent, sets:is_element(Square, UsedSquares), not sets:is_element(Square, Visited)].

find_used_squares_in_region([], _, Visited) ->
  Visited;
find_used_squares_in_region([Current | Remaining], UsedSquares, Visited) ->
  AdjacentUsedSquares = adjacent_new_used_squares(Current, UsedSquares, Visited),
  find_used_squares_in_region(Remaining ++ AdjacentUsedSquares, UsedSquares, sets:add_element(Current, Visited)).

count_regions([], Count) ->
  Count;
count_regions([Square | T], Count) ->
  UsedSquares = sets:from_list([Square | T]),
  SquaresInRegion = find_used_squares_in_region([Square], UsedSquares, sets:new()),
  Remaining = sets:subtract(UsedSquares, SquaresInRegion),
  count_regions(sets:to_list(Remaining), Count + 1).

part1() ->
  UsedSquares = grid_to_coordinates(create_grid(), 0, []),
  length(UsedSquares).

part2() ->
  UsedSquares = grid_to_coordinates(create_grid(), 0, []),
  count_regions(UsedSquares, 0).
