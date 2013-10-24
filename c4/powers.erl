%% Etudes
%% @author Neal Shrader <neal@digitalocean.com>

-module(powers).
-export([raise/2, nth_root/2]).

%% @doc Etude 4-3

%% raise(_,0) -> 1;
%% raise(X,1) -> X;
%% raise(X,N) when N > 0 ->
%%   X * raise(X,N-1);
%% raise(X,N) when N < 0 ->
%%   1.0 / raise(X, N * -1).

%% @doc Etude 4-4
-spec(raise(number(), number()) -> number()).

raise(_, 0) -> 1;
raise(X, Y) when Y < 0 ->
  1.0 / raise(X, -Y);
raise(X, Y) when Y > 0 ->
  raise(X, Y, 1).

-spec(raise(number(), number(), number()) -> number()).

raise(_, 0, Acc) -> Acc;
raise(X, Y, Acc) ->
  raise(X, Y-1, X*Acc).

%% @doc Etude 4-5
-spec(nth_root(number(), number()) -> number()).

nth_root(X, N) -> nth_root(X, N, X / 2.0).

-spec(nth_root(number(), number(), number()) -> number()).

nth_root(X, N, A) ->
  io:format("Current guess is ~p~n", [A]),
  F = raise(A, N) - X,
  Fprime = N * raise(A, N-1),
  Next = A - F / Fprime,
  Change = abs(Next - A),
  if
    Change < 1.0e-8 -> Next;
    true -> nth_root(X, N, Next)
  end.
