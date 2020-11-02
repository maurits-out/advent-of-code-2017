-module(duet_common).
-export([evaluate/2, update_register/3, value_of_register/2, read_instructions/0]).

read_instructions() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  Lines = [binary_to_list(B) || B <- Bins, byte_size(B) > 0],
  array:from_list([parse_line(L) || L <- Lines]).

parse_line(Line) ->
  [Instruction | Args] = string:lexemes(Line, " "),
  {list_to_atom(Instruction), Args}.

value_of_register(Expression, Registers) ->
  Register = lists:nth(1, Expression),
  maps:get(Register, Registers, 0).

update_register(Expression, Value, Registers) ->
  Register = lists:nth(1, Expression),
  Registers#{Register => Value}.

evaluate(Expression, Registers) ->
  case string:to_integer(Expression) of
    {error, _} -> value_of_register(Expression, Registers);
    {Value, _} -> Value
  end.


