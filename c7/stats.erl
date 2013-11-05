%% @author Neal Shrader <neal@digitalocean.com>
%% @doc Etude 7-3

-module(stats).
-export([mean/1, stdv/1]).

mean(List) ->
  lists:foldl(fun(X, Sum) -> X + Sum end, 0, List) / length(List).

stdv(List) ->
  {Sum, SumSq} = lists:foldl(fun(X, Acc) -> {SumA, SumSqA} = Acc, {SumA+X, SumSqA+(X*X)} end, 
    {0,0}, List),
  N = length(List),
  math:sqrt((N * SumSq - Sum * Sum)/(N * (N - 1))).
