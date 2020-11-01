-module(duet).
-export([part1/0]).

read_instructions() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  Lines = [binary_to_list(B) || B <- Bins, byte_size(B) > 0],
  array:from_list([parse_line(L) || L <- Lines]).

parse_line(Line) ->
  [Instruction | Args] = string:lexemes(Line, " "),
  {list_to_atom(Instruction), Args}.

register_value(Expression, Registers) ->
  Register = lists:nth(1, Expression),
  maps:get(Register, Registers, 0).

update_register(Expression, Value, Registers) ->
  Register = lists:nth(1, Expression),
  Registers#{Register => Value}.

evaluate(Expression, Registers) ->
  case string:to_integer(Expression) of
    {error, _} -> register_value(Expression, Registers);
    {Value, _} -> Value
  end.

execute(Instructions, PC, Registers, LastPlayed) ->
  case array:get(PC, Instructions) of
    {snd, [X]} ->
      execute(Instructions, PC + 1, Registers, evaluate(X, Registers));
    {set, [X, Y]} ->
      Value = evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), LastPlayed);
    {add, [X, Y]} ->
      Value = register_value(X, Registers) + evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), LastPlayed);
    {mul, [X, Y]} ->
      Value = register_value(X, Registers) * evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), LastPlayed);
    {mod, [X, Y]} ->
      Value = register_value(X, Registers) rem evaluate(Y, Registers),
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

part1() ->
  Instructions = read_instructions(),
  execute(Instructions, 0, #{}, none).
