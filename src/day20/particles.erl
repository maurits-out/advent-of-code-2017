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

parse_input() ->
  [parse_line(L) || L <- read_lines()].

add(#vector{x = X1, y = Y1, z = Z1}, #vector{x = X2, y = Y2, z = Z2}) ->
  #vector{x = X1 + X2, y = Y1 + Y2, z = Z1 + Z2}.

update_particle(#particle{position = Position, velocity = Velocity, acceleration = Acceleration} = Particle) ->
  NewVelocity = add(Velocity, Acceleration),
  NewPosition = add(Position, NewVelocity),
  Particle#particle{position = NewPosition, velocity = NewVelocity}.

update_particles(Particles) ->
  [update_particle(P) || P <- Particles].

iterate(Particles, Iterations) ->
  lists:foldl(fun(_, P) -> update_particles(P) end, Particles, lists:seq(1, Iterations)).

distance_to_origin(#particle{position = Position}) ->
  #vector{x = X, y = Y, z = Z} = Position,
  abs(X) + abs(Y) + abs(Z).

find_particle_closest_to_origin(Particles) ->
  ParticlesWithID = lists:zip(lists:seq(0, length(Particles) - 1), Particles),
  Sorted = lists:sort(fun({_, P1}, {_, P2}) -> distance_to_origin(P1) =< distance_to_origin(P2) end, ParticlesWithID),
  {ID, _} = lists:nth(1, Sorted),
  ID.

part1() ->
  Particles = parse_input(),
  ID = find_particle_closest_to_origin(iterate(Particles, 1000)),
  io:format("Particle closest to origin (part 1): ~p~n", [ID]).
