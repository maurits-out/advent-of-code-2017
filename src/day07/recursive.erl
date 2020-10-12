-module(recursive).
-export([part2/0]).

-record(program, {name, weight, children}).

read_lines_from_input() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

parse_children(ChildrenString) ->
  case re:run(ChildrenString, " -> (.+)", [{capture, all_but_first, list}]) of
    {match, [Children]} -> string:lexemes(Children, ", ");
    _ -> []
  end.

parse_line(Line) ->
  {match, [Program, Weight, ChildrenString]} = re:run(Line, "(\\w+).+\\((\\d+)\\)(.*)", [{capture, all_but_first, list}]),
  #program{name = Program, weight = element(1, string:list_to_integer(Weight)), children = parse_children(ChildrenString)}.

parse_input() ->
  Programs = [parse_line(L) || L <- read_lines_from_input()],
  maps:from_list([{P#program.name, P} || P <- Programs]).

total_weight(Name, Programs) ->
  Program = maps:get(Name, Programs),
  Program#program.weight + lists:sum([total_weight(Child, Programs) || Child <- Program#program.children]).

balanced(Name, Programs) ->
  Program = maps:get(Name, Programs),
  Weights = sets:from_list([total_weight(Child, Programs) || Child <- Program#program.children]),
  sets:size(Weights) =< 1.

find_unbalanced_disc(Name, Programs) ->
  Program = maps:get(Name, Programs),
  Children = Program#program.children,
  UnbalancedChildren = lists:filtermap(fun(Child) -> find_unbalanced_disc(Child, Programs) end, Children),
  case UnbalancedChildren of
    [] ->
      case not balanced(Name, Programs) of
        true ->
          {true, Name};
        false ->
          false
      end;
    [N] ->
      {true, N}
  end.

total_weights_of_child_programs(UnbalancedName, Programs) ->
  Program = maps:get(UnbalancedName, Programs),
  [{Child, total_weight(Child, Programs)} || Child <- Program#program.children].

part2() ->
  Programs = parse_input(),
  {true, UnbalancedNode} = find_unbalanced_disc("airlri", Programs),
  total_weights_of_child_programs(UnbalancedNode, Programs).
