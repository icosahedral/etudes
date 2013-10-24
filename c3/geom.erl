%% @author Neal Shrader <neal@digitalocean.com>
%% @doc Calculates area of various shapes
-module(geom).
-compile([debug_info]).
-export([area/1]).

%% @doc External interface to calculate area of things.
-spec(area({atom(), number(), number()}) -> number()).
area({Name, X, Y}) -> area(Name, X, Y).

%% @doc Etudes 3-1 through 3-4
-spec(area(atom(), number(), number()) -> number()).
area(rectangle, X, Y) when X > 0, Y > 0 ->
    X * Y;
area(triangle, X, Y) when X > 0, Y > 0 ->
    X * Y / 2.0;
area(ellipse, X, Y) when X > 0, Y > 0 ->
    math:pi() * X * Y;
area(_, _, _) ->
    0.
