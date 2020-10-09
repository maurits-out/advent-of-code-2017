-module(trampolines).
-export([part1/0, part2/0]).

instructions() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  Lists = [binary_to_list(B) || B <- Bins, byte_size(B) > 0],
  array:from_list([element(1, string:list_to_integer(L)) || L <- Lists]).

count_steps(Instructions, PC, Delta, Steps) ->
  case 0 =< PC andalso PC < array:size(Instructions) of
    true ->
      Offset = array:get(PC, Instructions),
      Updated = array:set(PC, Offset + Delta(Offset), Instructions),
      count_steps(Updated, PC + Offset, Delta, Steps + 1);
    false ->
      Steps
  end.

update_offset(Offset) ->
  if
    Offset >= 3 -> -1;
    true -> 1
  end.

part1() ->
  count_steps(instructions(), 0, fun(_) -> 1 end, 0).

part2() ->
  count_steps(instructions(), 0, fun update_offset/1, 0).
