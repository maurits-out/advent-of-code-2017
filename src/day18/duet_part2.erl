-module(duet_part2).
-export([run/0]).

-record(state, {id, pc = 0, registers, partner_pid, num_values_sent = 0}).

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

execute(Instructions, #state{pc = PC, registers = Registers} = State) ->
  case array:get(State#state.pc, Instructions) of
    {snd, [X]} ->
      State#state.partner_pid ! evaluate(X, Registers),
      execute(Instructions, State#state{pc = PC + 1, num_values_sent = State#state.num_values_sent + 1});
    {set, [X, Y]} ->
      Value = evaluate(Y, Registers),
      execute(Instructions, State#state{pc = PC + 1, registers = update_register(X, Value, Registers)});
    {add, [X, Y]} ->
      Value = value_of_register(X, Registers) + evaluate(Y, Registers),
      execute(Instructions, State#state{pc = PC + 1, registers = update_register(X, Value, Registers)});
    {mul, [X, Y]} ->
      Value = value_of_register(X, Registers) * evaluate(Y, Registers),
      execute(Instructions, State#state{pc = PC + 1, registers = update_register(X, Value, Registers)});
    {mod, [X, Y]} ->
      Value = value_of_register(X, Registers) rem evaluate(Y, Registers),
      execute(Instructions, State#state{pc = PC + 1, registers = update_register(X, Value, Registers)});
    {rcv, [X]} ->
      receive
        Value ->
          execute(Instructions, State#state{pc = PC + 1, registers = update_register(X, Value, Registers)})
      after 100 ->
        io:format("Program with ID ~p sent ~p values~n", [State#state.id, State#state.num_values_sent])
      end;
    {jgz, [X, Y]} ->
      Offset = case evaluate(X, Registers) of
                 Value when Value > 0 -> evaluate(Y, Registers);
                 _ -> 1
               end,
      execute(Instructions, State#state{pc = PC + Offset})
  end.

start(ProgramID, Instructions) ->
  PartnerProgramPid = receive Pid -> Pid end,
  State = #state{id = ProgramID, registers = #{$p => ProgramID}, partner_pid = PartnerProgramPid},
  execute(Instructions, State).

run() ->
  Instructions = read_instructions(),
  Pid0 = spawn(fun() -> start(0, Instructions) end),
  Pid1 = spawn(fun() -> start(1, Instructions) end),
  Pid0 ! Pid1,
  Pid1 ! Pid0,
  ok.
