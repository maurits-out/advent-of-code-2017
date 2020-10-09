-module(passphrases).
-export([part1/0, part2/0]).

read_lines_from_input() ->
  {ok, Bin} = file:read_file("input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

no_duplicate_words(Words) ->
  UniqueWords = sets:from_list(Words),
  length(Words) =:= sets:size(UniqueWords).

no_anagrams(Words) ->
  no_duplicate_words([lists:sort(Word) || Word <- Words]).

words(Line) ->
  string:lexemes(Line, [$\ ]).

count_valid_passphrases(Predicate) ->
  length([L || L <- read_lines_from_input(), Predicate(words(L))]).

part1() ->
  count_valid_passphrases(fun no_duplicate_words/1).

part2() ->
  count_valid_passphrases(fun no_anagrams/1).
