%% @author Neal Shrader <neal@digitalocean.com>
%% Etude 7-4 - julian/1 update

-module(dates).
-export([date_parts/1, julian/1, is_leap_year/1]).

%% @doc splits date into three strings
-spec(date_parts(string()) -> {integer(), integer(), integer()}).
date_parts(Date) -> 
  [YearStr, MonStr, DayStr] = re:split(Date, "[\-]", [{return,list}]),
  [element(1, string:to_integer(YearStr)),
    element(1, string:to_integer(MonStr)),
    element(1, string:to_integer(DayStr))].

%% @doc tell us if year is a leap year
-spec(is_leap_year(integer()) -> boolean()).
is_leap_year(Year) ->
  (Year rem 4 == 0 andalso Year rem 100 /= 0)
  orelse (Year rem 400 == 0).

%% @doc returns number of days into year for a date

-spec(julian(string()) -> integer()).
julian(Date) ->
  [Year, Mon, Day] = date_parts(Date),
  case is_leap_year(Year) of
    true  -> DayList = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    false -> DayList = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  end,
  {TruncDayList, _} = lists:split(Mon-1, DayList),
  lists:foldl(fun(X, A) -> X+A end, 0, TruncDayList) + Day.

