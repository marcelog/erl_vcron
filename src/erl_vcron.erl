%%% @doc Minimal vixie-cron expression parser. Based on the information in
%%% https://en.wikipedia.org/wiki/Cron
%%%
%%% @todo Add support for "L", "W", and "?"
%%% @todo Check for invalid expressions
%%%
%%% Copyright 2017 Marcelo Gornstein &lt;marcelog@@gmail.com&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Marcelo Gornstein <marcelog@gmail.com>
%%% @author Marcelo Gornstein <marcelog@gmail.com>
%%%
-module(erl_vcron).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([applies/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Given a datetime() and a vixie-like cron expression, will return true
%% if the expression covers the datetime().
-spec applies(calendar:datetime(), string()) -> boolean().
applies(
  _DateTime = {{Year, Month, Day}, {Hour, Minute, _Second}},
  Expression
) ->
  [
    MinuteExpression,
    HourExpression,
    DoMExpression,
    MonthExpression,
    DoWExpression
  ]  = string:tokens(Expression, " "),
  DoW = case calendar:day_of_the_week(Year, Month, Day) of
    7 -> 0;
    DoW_ -> DoW_
  end,
  %last_day_of_the_month(Year, Month)
  applies_standard(Minute, 60, MinuteExpression) andalso
  applies_standard(Hour, 60, HourExpression) andalso
  applies_standard(Day, 31, DoMExpression) andalso
  applies_standard(Month, 12, MonthExpression) andalso
  applies_standard(DoW, 6, DoWExpression).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Tries to match a number for expressions like "*/1", "X-Y", "X", "X,Y,Z"
-spec applies_standard(
  non_neg_integer(), pos_integer(), string()
) -> boolean().
applies_standard(_Number, _Max, "*") ->
  true;

% In vixie-cron, slashes can be combined with ranges to specify step values.
% For example, */5 in the minutes field indicates every 5 minutes. It is
% shorthand for the more verbose POSIX form 5,10,15,20,25,30,35,40,45,50,55,00.
% POSIX does not define a use for slashes; its rationale (commenting on a
% BSD extension) notes that the definition is based on System V format
% but does not exclude the possibility of extensions
applies_standard(Number, Max, [$*, $/|Interval]) ->
  List = generate_from_interval(list_to_integer(Interval), Max),
  is_in_list(Number, List);

applies_standard(Number, _Max, String) ->
  % Commas are used to separate items of a list. For example,
  % using "MON,WED,FRI" in the 5th field (day of week) means
  % Mondays, Wednesdays and Fridays.
  IsList = string:tokens(String, ","),
  % Hyphens define ranges. For example, 2000-2010
  % indicates every year between 2000 and 2010, inclusive.
  IsRange = string:tokens(String, "-"),
  List = if
    length(IsList) > 1 ->
      [ list_to_integer(M) || M <- IsList];
    length(IsRange) > 1 ->
      [Min, Max] = IsRange,
      lists:seq(list_to_integer(Min), list_to_integer(Max));
    true ->
      [list_to_integer(String)]
  end,
  is_in_list(Number, List).

%% @doc Returns a list from an expression like */5.
-spec generate_from_interval(
  pos_integer(), pos_integer()
) -> [non_neg_integer()].
generate_from_interval(Interval, Max) ->
  generate_from_interval(0, Max, Interval, []).

%% @doc Tail recursion for generate_from_interval/2.
-spec generate_from_interval(
  non_neg_integer(), pos_integer(), pos_integer(), [non_neg_integer()]
) -> [non_neg_integer()].
generate_from_interval(Current, Max, _Interval, Acc) when Current > Max ->
  Acc;

generate_from_interval(Current, Max, Interval, Acc)
  when (Current + Interval) =:= Max ->
  % @todo: This is actually a kludge. Will add "Max" which doesn't make
  % sense for hours/minutes (will include 0-60), but will make it work for
  % months (1-12). For hours/minutes, including Max (which is 60) doesn't make
  % sense, but it does makes sense for months (they go from 1 to 12, although
  % there is no such thing as the month 0 in Erlang).
  [0, Max|Acc];

generate_from_interval(Current, Max, Interval, Acc) ->
  NewCurrent = Current + Interval,
  generate_from_interval(NewCurrent, Max, Interval, [NewCurrent|Acc]).

%% @doc true if the given list contains the element.
-spec is_in_list(non_neg_integer(), [non_neg_integer()]) -> boolean().
is_in_list(Element, List) ->
  Result = lists:filter(fun(E) -> Element =:= E end, List),
  length(Result) > 0.
