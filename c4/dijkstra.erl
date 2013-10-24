%% Etude 4-2
%% @author Neal Shrader <neal@digitalocean.com>

-module(dijkstra).
-compile([debug_info]).
-export([gcd/2]).

-spec(gcd(integer(), integer()) -> integer()).

gcd(X,X) -> X;
gcd(X,Y) when X > Y ->
  gcd(X-Y, Y);
gcd(X,Y) when X < Y ->
  gcd(X, Y-X).
