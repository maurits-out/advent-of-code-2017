-module(inverse_captcha_part2).
-export([solve_captcha/0]).
-import(file, [read_file/1]).
-import(string, [trim/1]).
-import(lists, [split/2]).

char_to_int(X) -> X - $0.

solve([], [], Acc) -> Acc * 2;
solve([X | T], [X | U], Acc) -> solve(T, U, Acc + char_to_int(X));
solve([_ | T], [_ | U], Acc) -> solve(T, U, Acc).

solve_captcha() ->
  {ok, File} = read_file("input.txt"),
  Content = trim(unicode:characters_to_list(File)),
  {List1, List2} = split(length(Content) div 2, Content),
  solve(List1, List2, 0).
