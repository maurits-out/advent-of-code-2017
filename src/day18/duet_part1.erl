-module(duet_part1).
-import(duet_common, [read_instructions/0, value_of_register/2, update_register/3, evaluate/2]).
-export([run/0]).

execute(Instructions, PC, Registers, LastPlayed) ->
  case array:get(PC, Instructions) of
    {snd, [X]} ->
      execute(Instructions, PC + 1, Registers, evaluate(X, Registers));
    {set, [X, Y]} ->
      Value = evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), LastPlayed);
    {add, [X, Y]} ->
      Value = value_of_register(X, Registers) + evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), LastPlayed);
    {mul, [X, Y]} ->
      Value = value_of_register(X, Registers) * evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), LastPlayed);
    {mod, [X, Y]} ->
      Value = value_of_register(X, Registers) rem evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), LastPlayed);
    {rcv, [X]} ->
      case evaluate(X, Registers) of
        0 -> execute(Instructions, PC + 1, Registers, LastPlayed);
        _ -> LastPlayed
      end;
    {jgz, [X, Y]} ->
      Offset = case evaluate(X, Registers) of
                 Value when Value > 0 -> evaluate(Y, Registers);
                 _ -> 1
               end,
      execute(Instructions, PC + Offset, Registers, LastPlayed)
  end.

run() ->
  Instructions = read_instructions(),
  execute(Instructions, 0, #{}, none).
