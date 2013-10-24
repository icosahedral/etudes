%% @author Neal Shrader <neal@digitalocean.com>
%% @doc Etude 6-5: generating random list of lists

-module(non_fp).
-export([generate_teeth/2]).

%% @doc generate a list of tooth depths based on a sting representing present and missing teeth
%% and the probability that there's something wrong with one of them.
-spec(generate_teeth(string(), float()) -> list()).
generate_teeth(ToothString, Prob) ->
  random:seed(now()),
  generate_teeth(ToothString, Prob, []).

-spec(generate_teeth(list(), float(), list()) -> list()).
generate_teeth([], _, Acc) -> lists:reverse(Acc);
generate_teeth([$F | RemTeeth], Prob, Acc) -> generate_teeth(RemTeeth, Prob, [[0] | Acc]);
generate_teeth([$T | RemTeeth], Prob, Acc) ->
  Tooth = generate_tooth(Prob),
  generate_teeth(RemTeeth, Prob, [Tooth | Acc]).

%% @doc generate list of depth values for a single tooth
-spec(generate_tooth(float()) -> list()).
generate_tooth(Prob) ->
  ToothState = random:uniform(),
  case ToothState > Prob of
    true  -> BaseDepth = 2;
    false -> BaseDepth = 3
  end,
  generate_tooth(BaseDepth, 6, []).

-spec(generate_tooth(integer(), integer(), list()) -> list()).
generate_tooth(_, 0, Acc) -> Acc;
generate_tooth(BaseDepth, NumTeeth, Acc) ->
  Jitter = random:uniform(3) - 2,
  generate_tooth(BaseDepth, NumTeeth - 1, [BaseDepth + Jitter | Acc]).

