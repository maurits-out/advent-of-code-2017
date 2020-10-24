-module(plumber).
-export([part1/0, part2/0]).

part1() ->
  Graph = read_graph(),
  sets:size(find_group(0, Graph)).

part2() ->
  Graph = read_graph(),
  Vertices = maps:keys(Graph),
  count_groups(Vertices, 0, Graph).

read_lines_from_input() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

parse_neighbors(NeighborsString) ->
  [list_to_integer(S) || S <- string:lexemes(NeighborsString, ", ")].

parse_line(Line) ->
  {match, [Program, NeighborsString]} = re:run(Line, "(\\d+) <-> (.*)", [{capture, all_but_first, list}]),
  {list_to_integer(Program), parse_neighbors(NeighborsString)}.

read_graph() ->
  Lines = read_lines_from_input(),
  maps:from_list([parse_line(L) || L <- Lines]).

find_group([], Group, _) ->
  Group;
find_group([V | T], Group, Graph) ->
  case sets:is_element(V, Group) of
    true ->
      find_group(T, Group, Graph);
    false ->
      find_group(lists:append(T, maps:get(V, Graph)), sets:add_element(V, Group), Graph)
  end.

find_group(V, Graph) ->
  find_group([V], sets:new(), Graph).

count_groups([], Count, _) ->
  Count;
count_groups(Vertices, Count, Graph) ->
  V = lists:nth(1, Vertices),
  Group = find_group(V, Graph),
  Remaining = [W || W <- Vertices, not sets:is_element(W, Group)],
  count_groups(Remaining, Count + 1, Graph).
