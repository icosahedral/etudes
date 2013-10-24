%% @author Neal Shrader <neal@digitalocean>
%% @doc Etude 6-1 - 6-2

-module(stats).
-export([minimum/1, maximum/1, range/1]).

%% @doc Gives minimum number in a list
-spec(minimum(list()) -> number()).
minimum(List) ->
  [First | Rest] = List,
  minimum(Rest, First).

-spec(minimum(list(), number()) -> number()).
minimum([], Candidate) -> Candidate;
minimum(List, Candidate) ->
  [First | Rest] = List,
  if First >= Candidate -> minimum(Rest, Candidate);
     First < Candidate  -> minimum(Rest, First)
  end.

%% @doc Gives maximum number in list
-spec(maximum(list()) -> number()).
maximum(List) ->
  [First | Rest] = List,
  maximum(Rest, First).

-spec(maximum(list(), number()) -> number()).
maximum([], Candidate) -> Candidate;
maximum(List, Candidate) ->
  [First | Rest] = List,
  if First >= Candidate -> maximum(Rest, First);
     First < Candidate  -> maximum(Rest, Candidate)
  end.

%% @doc Gives min and max from a list
-spec(range(list()) -> list()).
range(List) ->
  Min = minimum(List),
  Max = maximum(List),
  [Min, Max].
