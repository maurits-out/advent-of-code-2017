-module(registers).
-export([part1/0, part2/0]).

read_input() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  Lines = [binary_to_list(B) || B <- Bins, byte_size(B) > 0],
  [parse_line(L) || L <- Lines].

parse_line(Line) ->
  [Reg1, Instr, Arg1, "if", Reg2, Comp, Arg2] = string:lexemes(Line, " "),
  Command = {Reg1, list_to_atom(Instr), list_to_integer(Arg1)},
  Condition = {Reg2, Comp, list_to_integer(Arg2)},
  {Command, Condition}.

register_value(Reg, State) ->
  maps:get(Reg, State, 0).

evaluate_condition({Reg, Comp, Value}, State) ->
  RegValue = register_value(Reg, State),
  case Comp of
    ">" -> RegValue > Value;
    ">=" -> RegValue >= Value;
    "==" -> RegValue == Value;
    "<" -> RegValue < Value;
    "<=" -> RegValue =< Value;
    "!=" -> RegValue /= Value
  end.

apply_command({Reg, Instr, Value}, {State, CurrentMax}) ->
  RegValue = register_value(Reg, State),
  NewValue = case Instr of
               inc -> RegValue + Value;
               dec -> RegValue - Value
             end,
  {State#{Reg => NewValue}, max(CurrentMax, NewValue)}.

apply_instruction({Command, Condition}, {State, CurrentMax}) ->
  case evaluate_condition(Condition, State) of
    true -> apply_command(Command, {State, CurrentMax});
    false -> {State, CurrentMax}
  end.

execute() ->
  lists:foldl(fun apply_instruction/2, {#{}, 0}, read_input()).

part1() ->
  {State, _} = execute(),
  lists:max(maps:values(State)).

part2() ->
  {_, Max} = execute(),
  Max.
