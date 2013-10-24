%% @author Neal Shrader <neal@digitalocean.com>
%% @doc Calculates area of a rectangle.
%% This module calculates the area of a rectangle.

-module(geom).
-export([area/2]).

%% @doc Calculates area of a rectangle.
%% @spec area(number(), number()) -> number()

-spec(area(number(), number()) -> number()).
area(Length, Width) -> Length * Width.
