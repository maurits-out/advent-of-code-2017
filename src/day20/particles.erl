-module(particles).
-export([part1/0]).
-record(vector, {x, y, z}).
-record(particle, {position, velocity, acceleration}).

read_lines() ->
  {ok, Bin} = file:read_file("src/day20/input.txt"),
  Bins = binary:split(Bin, <<$\n>>, [global]),
  [binary_to_list(B) || B <- Bins, byte_size(B) > 0].

parse_vector(Vector) ->
  [X, Y, Z] = string:lexemes(Vector, ","),
  #vector{
    x = list_to_integer(X),
    y = list_to_integer(Y),
    z = list_to_integer(Z)
  }.

parse_line(Line) ->
  {match, [Position, Velocity, Acceleration]} = re:run(Line, "p=<(.+)>, v=<(.+)>, a=<(.+)>",
    [{capture, all_but_first, list}]),
  #particle{
    position = parse_vector(Position),
    velocity = parse_vector(Velocity),
    acceleration = parse_vector(Acceleration)
  }.

parse_lines(Lines) ->
  [parse_line(L) || L <- Lines].

part1() ->
  Lines = read_lines(),
  io:format("~p~n", [parse_lines(Lines)]).
