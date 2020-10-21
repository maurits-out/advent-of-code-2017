-module(knot_hash).
-export([part1/0]).
-include_lib("eunit/include/eunit.hrl").

extract_subsections(Position, Length, List) ->
  case Position + Length - length(List) - 1 of
    Diff when Diff > 0 ->
      {lists:sublist(List, Position, length(List) - Position + 1), lists:sublist(List, Diff)};
    _ ->
      {lists:sublist(List, Position, Length), []}
  end.

reverse_subsections({S1, S2}) ->
  lists:split(length(S1), lists:reverse(S1 ++ S2)).

replace_subsections(Position, {R1, []}, List) ->
  FirstPart = lists:sublist(List, Position - 1) ++ R1,
  SecondPart = lists:sublist(List, length(FirstPart) + 1, length(List) - length(FirstPart)),
  FirstPart ++ SecondPart;
replace_subsections(_, {R1, R2}, List) ->
  R2 ++ lists:sublist(List, length(R2) + 1, length(List) - length(R1) - length(R2)) ++ R1.

reverse_section(Position, Length, List) ->
  {S1, S2} = extract_subsections(Position, Length, List),
  {R1, R2} = reverse_subsections({S1, S2}),
  replace_subsections(Position, {R1, R2}, List).

hash_round([], [N, M | _], _, _) ->
  N * M;
hash_round([Length | T], List, Position, SkipSize) ->
  UpdatedList = reverse_section(Position, Length, List),
  UpdatedPosition = (Position + Length + SkipSize - 1) rem length(List) + 1,
  hash_round(T, UpdatedList, UpdatedPosition, SkipSize + 1).

part1() ->
  PuzzleInput = [76, 1, 88, 148, 166, 217, 130, 0, 128, 254, 16, 2, 130, 71, 255, 229],
  hash_round(PuzzleInput, lists:seq(0, 255), 1, 0).

%% Tests

extract_subsections_test() ->
  ?assertEqual({[2, 3, 4], []}, extract_subsections(2, 3, [1, 2, 3, 4, 5])),
  ?assertEqual({[3, 4, 5], [1, 2]}, extract_subsections(3, 5, [1, 2, 3, 4, 5])),
  ?assertEqual({[5], [1, 2, 3]}, extract_subsections(5, 4, [1, 2, 3, 4, 5])),
  ?assertEqual({[1, 2, 3, 4, 5], []}, extract_subsections(1, 5, [1, 2, 3, 4, 5])).

reverse_subsections_test() ->
  ?assertEqual({[4, 3, 2], []}, reverse_subsections({[2, 3, 4], []})),
  ?assertEqual({[2, 1, 5], [4, 3]}, reverse_subsections({[3, 4, 5], [1, 2]})).

replace_subsections_test() ->
  ?assertEqual([1, 2, 4, 3, 5], replace_subsections(3, {[4, 3], []}, [1, 2, 3, 4, 5])),
  ?assertEqual([5, 4, 3, 2, 1], replace_subsections(1, {[5, 4, 3, 2, 1], []}, [1, 2, 3, 4, 5])),
  ?assertEqual([5, 4, 3, 2, 1], replace_subsections(3, {[3, 2, 1], [5, 4]}, [1, 2, 3, 4, 5])),
  ?assertEqual([4, 5, 3, 1, 2], replace_subsections(4, {[1, 2], [4, 5]}, [1, 2, 3, 4, 5])).

calculate_hash_test() ->
  ?assertEqual(12, hash_round([3, 4, 1, 5], [0, 1, 2, 3, 4], 1, 0)).
