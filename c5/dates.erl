%% @author Neal Shrader <neal@digitalocean.com
%% Etude 5-2: Splitting a date string into ints

-module(dates).
-export([date_parts/1]).

%% @doc splits date into three strings
-spec(date_parts(string()) -> {integer(), integer(), integer()}).
date_parts(Date) -> 
  [YearStr, MonStr, DayStr] = re:split(Date, "[\-]", [{return,list}]),
  [element(1, string:to_integer(YearStr)),
    element(1, string:to_integer(MonStr)),
    element(1, string:to_integer(DayStr))].
