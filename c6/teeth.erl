%% @author Neal Shrader <neal@digitalocean.com>
%% @doc Etude 6-4 please remember to floss

-module(teeth).
-export([alert/1]).

%% @doc alerts us to teeth that needs attention
-spec(alert(list()) -> list()).
alert(PocketDepths) -> alert(PocketDepths, []).

-spec(alert(list(), list()) -> list()).
alert([], ToothList) -> lists:reverse(ToothList);
alert([DepthListHead | DepthListRem], ToothList) ->
  ToothNum = 32 - length(DepthListRem),
  case tooth_is_clearly_rotting(DepthListHead) of
    true  -> alert(DepthListRem, [ToothNum | ToothList]);
    false -> alert(DepthListRem, ToothList)
  end.

%% @doc determines state of a tooth with science
-spec(tooth_is_clearly_rotting(list()) -> boolean()).
tooth_is_clearly_rotting([]) -> false;
tooth_is_clearly_rotting([Measure | _]) when Measure >= 4 -> true;
tooth_is_clearly_rotting([_ | RemList]) -> tooth_is_clearly_rotting(RemList).

