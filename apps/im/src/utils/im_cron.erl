-module(im_cron).

-include("im_common.hrl").

-export([init/0]).
-export([statistics_of_the_week_for_vendor_job/0]).
-export([change_cron_time/1]).

init() ->
  Jobs = case im:is_debug() of
    true ->
      [
        {{daily, {every, {1, sec}, {between, {00, 00, 00}, {23, 59, 59}}}}, fun(_Ref, _Time) ->
          archive_expired_tickets_job()
        end}
      ];
    false ->
      [
        {{daily, {every, {30, min}, {between, {00, 00, 00}, {23, 59, 59}}}}, fun(_Ref, _Time) ->
          archive_expired_tickets_job()
        end},
        {{weekly, mon, {2, am}}, fun(_Ref, _Time) ->
          statistics_of_the_week_for_vendor_job()
        end}
      ]
  end,

  lists:foreach(fun(Job) -> erlcron:cron(Job) end, Jobs),
  ok.

%% im_cron:statistics_of_the_week_for_vendor_job().
statistics_of_the_week_for_vendor_job() ->
  MillisecondsInOneDay = 24 * 60 * 60 * 1000,
  MillisecondsInOneWeek = MillisecondsInOneDay * 7,
  {{Year, Month, Day}, {_,_,_}} = dh_date:parse(dh_date:format("Y-m-d")),
  TimestampOfCurrentDayStart = im_common:datetime_to_timestamp({{Year, Month, Day}, {0,0,0}}),
  CurrentWeekDayNumber = case list_to_integer(dh_date:format("w", im_common:timestamp_to_datetime(sm:now()))) of %% 0 - Monday -> 6 - Sunday
    0 -> 6;
    D -> D - 1
  end,
  ThisWeekMondayStartTimestamp = TimestampOfCurrentDayStart - MillisecondsInOneDay * CurrentWeekDayNumber,
  PrevWeekMondayStartTimestamp = ThisWeekMondayStartTimestamp - MillisecondsInOneWeek,
  StartWeekTimestamp = PrevWeekMondayStartTimestamp,
  EndWeekTimestamp = PrevWeekMondayStartTimestamp,
%%  StartWeekTimestamp = 0,
%%  EndWeekTimestamp = 9577836800000,

  VendorSelector = {<<"vendorId">>, {<<"$exists">>, true}},
  BatchSize = 100,
  ctail_mongo:batch(im_usr, VendorSelector, BatchSize, fun(Users) ->
    lists:foreach(fun(#im_usr{id = UserId}) ->
      GroupByProductByDay = im_statistics:get_grouped_vendor_statistics(
        UserId,
        StartWeekTimestamp,
        EndWeekTimestamp,
        ?VENDOR_STATISTICS_GROUP_BY_PRODUCT_BY_DAY
      ),
      im_order_report:notify_statistics_of_the_week_for_vendor(UserId, GroupByProductByDay)
    end, Users)
  end).

archive_expired_tickets_job() ->
  ExpirePeriod = case im:is_debug() of
    true -> sm:now();
    false -> sm:now() - 43200000 %% 12h in microseconds
  end,
  Selector = {
    <<"date">>, {<<"$lt">>, ExpirePeriod},
    <<"type">>, ?FEED_POST_PAYMENT_TYPE_TICKET_WITH_DATE
  },
  ctail_mongo:exec(update, [<<"im_order">>, Selector, {<<"$set">>, {<<"status">>, ?ORDER_STATUS_ARCHIEVED}}, false, true]).

change_cron_time(#'ChangeCronTime'{timestamp = Timestamp}) ->
  case im:is_debug() of
    true ->
      DateTime = im_common:timestamp_to_datetime(Timestamp),
      erlcron:set_datetime(DateTime),
      _Delay = sm:now() - Timestamp,
      #'ChangeCronTimeResp'{timestamp = Timestamp};
    false ->
      #'ErrorResp'{code = ?ERROR_CODE_INVALID_MESSAGE}
  end.
