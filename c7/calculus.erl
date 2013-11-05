-module(calculus).
-export([derivative/2]).

derivative(Fun, X) ->
  Delta = 1.0e-10,
  (Fun(X + Delta) - Fun(X)) / Delta.
