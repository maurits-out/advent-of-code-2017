-module(inverse_captcha_part1).
-export([solve_captcha/0]).
-import(file, [read_file/1]).
-import(string, [trim/1]).

char_to_int(X) -> X - $0.

solve([X], X, Acc) -> Acc + char_to_int(X);
solve([_], _, Acc) -> Acc;
solve([X, X | T], First, Acc) -> solve([X | T], First, Acc + char_to_int(X));
solve([_, Y | T], First, Acc) -> solve([Y | T], First, Acc).

solve_captcha() ->
  {ok, File} = read_file("input.txt"),
  [H | T] = trim(unicode:characters_to_list(File)),
  solve([H | T], H, 0).