%%% @doc Tests
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
-module(erl_vcron_test).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matches_all_wildcards_test() ->
  assert({{2017, 09, 23}, {0, 1, 0}}, "* * * * *").

range_test() ->
  minute_range(),
  hour_range(),
  day_of_month_range(),
  month_range(),
  day_of_week_range().

interval_test() ->
  minute_interval(),
  hour_interval(),
  day_of_month_interval(),
  month_interval(),
  day_of_week_interval().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
day_of_week_interval() ->
  Expression = "* * * * */2",
  assert_list(day, lists:seq(2, 6), Expression),
  refute_list(day, [7, 8], Expression).

month_interval() ->
  Expression = "* * * */6 *",
  test_interval(month, 1, 12, 6, Expression).

day_of_month_interval() ->
  Expression = "* * */15 * *",
  test_interval(day, 1, 31, 15, Expression).

hour_interval() ->
  Expression = "* */2 * * *",
  test_interval(hour, 0, 59, 2, Expression).

minute_interval() ->
  Expression = "*/5 * * * *",
  test_interval(minute, 0, 59, 5, Expression).

minute_range() ->
  Expression = "57-59 * * * *",
  refute_list(minute, lists:seq(0, 56), Expression),
  assert_list(minute, lists:seq(57, 59), Expression).

hour_range() ->
  Expression = "* 10-13 * * *",
  refute_list(hour, lists:seq(0, 9) ++ lists:seq(14, 23), Expression),
  assert_list(hour, lists:seq(10, 13), Expression).

day_of_month_range() ->
  Expression = "* * 10-15 * *",
  refute_list(day, lists:seq(1, 9) ++ lists:seq(16, 31), Expression),
  assert_list(day, lists:seq(10, 15), Expression).

month_range() ->
  Expression = "* * * 5-6 *",
  refute_list(month, lists:seq(1, 4) ++ lists:seq(7, 12), Expression),
  assert_list(month, [5, 6], Expression).

day_of_week_range() ->
  Expression = "* * * * 1-5",
  refute_list(day, [7, 8], Expression),
  assert_list(day, lists:seq(2, 6), Expression).

refute_list(What, List, Expression) ->
  test_for_list(false, What, List, Expression).

assert_list(What, List, Expression) ->
  test_for_list(true, What, List, Expression).

test_interval(What, Min, Max, Interval, Expression) ->
  io:format(
    "Interval ~p (~p-~p) every ~p against ~p~n",
    [What, Min, Max, Interval, Expression
  ]),
  False = lists:filter(
    fun(E) -> E rem Interval =/= 0 end,
    lists:seq(Min, Max)
  ),
  io:format("False list: ~p~n", [False]),
  refute_list(What, False, Expression),

  True = lists:filter(
    fun(E) -> E rem Interval =:= 0 end,
    lists:seq(Min, Max)
  ),
  io:format("True list: ~p~n", [True]),
  assert_list(What, True, Expression).

test_for_list(Result, What, List, Expression) ->
  _ = lists:foreach(
    fun(X) ->
      DateTime = case What of
        minute -> {{2017, 10, 1}, {0, X, 0}};
        hour -> {{2017, 10, 1}, {X, 0, 0}};
        day -> {{2017, 10, X}, {0, 0, 0}};
        month -> {{2017, X, 1}, {0, 0, 0}}
      end,
      test_for(Result, DateTime, Expression)
    end,
    List
  ).

assert(DateTime, Expression) ->
  test_for(true, DateTime, Expression).

%refute(DateTime, Expression) ->
%  test_for(false, DateTime, Expression).

test_for(Result, DateTime, Expression) ->
  RealResult = erl_vcron:applies(DateTime, Expression),
  io:format(
    "Expecting ~p against ~p to be ~p (was ~p) ~n",
    [DateTime, Expression, Result, RealResult]
  ),
  RealResult = Result.