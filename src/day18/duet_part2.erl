-module(duet_part2).
-export([run/0]).

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

execute(ProgramID, Instructions, PC, Registers, PartnerProgramPid, NumValuesSent) ->
  case array:get(PC, Instructions) of
    {snd, [X]} ->
      PartnerProgramPid ! evaluate(X, Registers),
      execute(ProgramID, Instructions, PC + 1, Registers, PartnerProgramPid, NumValuesSent + 1);
    {set, [X, Y]} ->
      Value = evaluate(Y, Registers),
      execute(ProgramID, Instructions, PC + 1, update_register(X, Value, Registers), PartnerProgramPid, NumValuesSent);
    {add, [X, Y]} ->
      Value = register_value(X, Registers) + evaluate(Y, Registers),
      execute(ProgramID, Instructions, PC + 1, update_register(X, Value, Registers), PartnerProgramPid, NumValuesSent);
    {mul, [X, Y]} ->
      Value = register_value(X, Registers) * evaluate(Y, Registers),
      execute(ProgramID, Instructions, PC + 1, update_register(X, Value, Registers), PartnerProgramPid, NumValuesSent);
    {mod, [X, Y]} ->
      Value = register_value(X, Registers) rem evaluate(Y, Registers),
      execute(ProgramID, Instructions, PC + 1, update_register(X, Value, Registers), PartnerProgramPid, NumValuesSent);
    {rcv, [X]} ->
      receive
        Value ->
          execute(ProgramID, Instructions, PC + 1, update_register(X, Value, Registers), PartnerProgramPid, NumValuesSent)
      after 100 ->
        io:format("Program with ID ~p sent ~p values~n", [ProgramID, NumValuesSent])
      end;
    {jgz, [X, Y]} ->
      Offset = case evaluate(X, Registers) of
                 Value when Value > 0 -> evaluate(Y, Registers);
                 _ -> 1
               end,
      execute(ProgramID, Instructions, PC + Offset, Registers, PartnerProgramPid, NumValuesSent)
  end.

start(ProgramID, Instructions) ->
  PartnerProgramPid = receive Pid -> Pid end,
  execute(ProgramID, Instructions, 0, #{$p => ProgramID}, PartnerProgramPid, 0).

run() ->
  Instructions = read_instructions(),
  Pid0 = spawn(fun() -> start(0, Instructions) end),
  Pid1 = spawn(fun() -> start(1, Instructions) end),
  Pid0 ! Pid1,
  Pid1 ! Pid0.
