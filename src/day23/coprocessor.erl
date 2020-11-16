-module(coprocessor).
-export([part1/0, part2/0]).

read_instructions() ->
  {ok, Bin} = file:read_file("src/day23/input.txt"),
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

execute(Instructions, PC, Registers, MulCount) ->
  case array:get(PC, Instructions) of
    {set, [X, Y]} ->
      Value = evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), MulCount);
    {sub, [X, Y]} ->
      Value = value_of_register(X, Registers) - evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), MulCount);
    {mul, [X, Y]} ->
      Value = value_of_register(X, Registers) * evaluate(Y, Registers),
      execute(Instructions, PC + 1, update_register(X, Value, Registers), MulCount + 1);
    {jnz, [X, Y]} ->
      Offset = case evaluate(X, Registers) of
                 Value when Value /= 0 -> evaluate(Y, Registers);
                 _ -> 1
               end,
      execute(Instructions, PC + Offset, Registers, MulCount);
    undefined ->
      MulCount
  end.

is_non_prime(N) ->
  length([M || M <- lists:seq(2, trunc(math:sqrt(N))), N rem M == 0]) > 0.

count_non_prime(Start) ->
  length([M || M <- lists:seq(Start, Start + 17000, 17), is_non_prime(M)]).

part1() ->
  Instructions = read_instructions(),
  MulCount = execute(Instructions, 0, #{}, 0),
  io:format("The mul instruction is invoked ~p times.~n", [MulCount]).

part2() ->
  io:format("Number of non-prime numbers found: ~p.~n", [count_non_prime(79 * 100 + 100000)]).
