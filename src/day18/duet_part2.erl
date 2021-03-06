-module(duet_part2).
-export([run/0]).
-import(duet_common, [read_instructions/0, value_of_register/2, update_register/3, evaluate/2]).
-record(state, {id, pc = 0, registers, partner_pid, num_values_sent = 0}).

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
