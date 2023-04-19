-module(im_statistics).

-include("im_common.hrl").
-include_lib("smoothie/include/sm.hrl").

-export([get_vendor_statistics/2, get_grouped_vendor_statistics/4]).

get_vendor_statistics(#'VendorStatistics'{ref = Ref, from = From, to = To, groupBy = GroupBy}, VendorId) ->
  case im_roster_chat:get(VendorId) =/= undefined of
    true ->
      Items = get_grouped_vendor_statistics(VendorId, From, To, GroupBy),
      #'VendorStatisticsResp' {
        ref = Ref,
        items = Items
      };
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

get_grouped_vendor_statistics(VendorId, From, To, ?VENDOR_STATISTICS_GROUP_BY_PRODUCT) ->
  Selector = {
    <<"vendorId">>, im_common:parse_id(VendorId),
    <<"purchaseDate">>, {<<"$gte">>, From, <<"$lt">>, To}
  },
  Orders = ctail_mongo:find(im_order, Selector, 0),
  lists:foldl(fun(Order = #im_order{}, Acc) ->
    {ok, Product} = ctail:get(im_feed_post, im_common:parse_id(Order#im_order.productId)),
    {Id, Name} = {Order#im_order.productId, Product#im_feed_post.title},

    add_statistic_item_to_acc(Order, im_common:format_id(Id), Name, Acc)
  end, [], Orders);

get_grouped_vendor_statistics(VendorId, From, To, ?VENDOR_STATISTICS_GROUP_BY_DAY) ->
  Selector = {
    <<"vendorId">>, im_common:parse_id(VendorId),
    <<"purchaseDate">>, {<<"$gte">>, From, <<"$lt">>, To}
  },
  Orders = ctail_mongo:find(im_order, Selector, 0),
  lists:foldl(fun(Order = #im_order{purchaseDate = PurchaseDate}, Acc) ->
    {{Y,M,D}, _} = im_common:timestamp_to_datetime(PurchaseDate),
    {Id, Name} = {dh_date:format("Y-m-d", {{Y,M,D}, {0,0,0}}), dh_date:format("l", {{Y,M,D}, {0,0,0}})},
    add_statistic_item_to_acc(Order, Id, Name, Acc)
  end, [], Orders);
get_grouped_vendor_statistics(VendorId, From, To, ?VENDOR_STATISTICS_GROUP_BY_PRODUCT_BY_DAY) ->
  Pipeline = [
    {<<"$match">>, {
      <<"vendorId">>, im_common:parse_id(VendorId),
      <<"purchaseDate">>, {<<"$gte">>, From, <<"$lt">>, To}
    }},
    {<<"$project">>, {
      <<"productId">>, 1,
      <<"total">>, 1,
      <<"qty">>, 1,
      <<"purchaseDate">>, 1,
      <<"purchaseDateFormatted">>, {
        <<"$dateToString">>, { <<"format">>, <<"%Y-%m-%d">>, <<"date">>, {
          <<"$toDate">>, <<"$purchaseDate">>
        }}
      },
      <<"dayOfWeek">>, {
        <<"$dayOfWeek">>, [{
          <<"$toDate">>, <<"$purchaseDate">>
        }]
      }
    }},
    {<<"$group">>, {
      <<"_id">>, {
        <<"productId">>, <<"$productId">>,
        <<"purchaseDateFormatted">>, <<"$purchaseDateFormatted">>,
        <<"dayOfWeek">>, <<"$dayOfWeek">>
      },
      <<"totalQty">>, {
        <<"$sum">>, <<"$qty">>
      },
      <<"totalPrice">>, {
        <<"$sum">>, <<"$total">>
      }
    }}
  ],

  case ctail_mongo:aggregate(im_order, Pipeline) of
    {ok, []} -> [];
    {ok, Data} ->
      lists:foldl(fun(Map, Acc) ->
        Map1 = maps:get(<<"_id">>, Map),

        DateFormatted = maps:get(<<"purchaseDateFormatted">>, Map1), %% 2019-04-27
        DayOfWeek = format_mongodb_day_week(maps:get(<<"dayOfWeek">>, Map1)), %% Format Int to String ( 1 - Sunday -> 7 - Saturday )
        ProductId = maps:get(<<"productId">>, Map1),
        {ok, Product} = ctail:get(im_feed_post, im_common:parse_id(ProductId)),
        ProductName = Product#im_feed_post.title,
        {<<"paymentInfo">>, PaymentInfoProps} = Product#im_feed_post.paymentInfo,
        Currency = proplists:get_value(<<"currency">>, PaymentInfoProps),

        TotalPrice = maps:get(<<"totalPrice">>, Map),
        TotalQty = maps:get(<<"totalQty">>, Map),

        Id = ProductId,

        case lists:filter(fun(#'StatisticEntity'{id = Id1}) -> Id1 =:= Id end, Acc) of
          [] ->
            [#'StatisticEntity'{
              id = im_common:format_id(Id),
              name = ProductName,
              qty = TotalQty,
              total = TotalPrice,
              currency = Currency,
              items = [
                #'StatisticEntity'{
                  id = DateFormatted,
                  name = DayOfWeek,
                  qty = TotalQty,
                  total = TotalPrice,
                  currency = Currency
                }
              ]
            }|Acc];
          [#'StatisticEntity'{}] ->
            lists:foldl(fun(ProductStatistic = #'StatisticEntity'{id=Id2}, Acc1) ->
              case Id2 of
                Id ->
                  DayStatisticEntity = #'StatisticEntity'{
                    id = DateFormatted,
                    name = DayOfWeek,
                    qty = TotalQty,
                    total = TotalPrice,
                    currency = Currency
                  },
                  [ProductStatistic#'StatisticEntity'{
                    items = [DayStatisticEntity | ProductStatistic#'StatisticEntity'.items],
                    total = ProductStatistic#'StatisticEntity'.total + TotalPrice,
                    qty = ProductStatistic#'StatisticEntity'.qty + TotalQty
                  }|Acc1];
                _ ->
                  [ProductStatistic|Acc1]
              end
            end, [], Acc)
        end
      end, [], Data);
    Error -> Error
  end.

add_statistic_item_to_acc(Order = #im_order{}, Id, Name, Acc) ->
  {ok, Product} = ctail:get(im_feed_post, im_common:parse_id(Order#im_order.productId)),
  {<<"paymentInfo">>, PaymentInfoProps} = Product#im_feed_post.paymentInfo,
  Currency = proplists:get_value(<<"currency">>, PaymentInfoProps),

  Entity = #'StatisticEntity'{
    id = im_common:format_id(Order#im_order.id),
    qty = Order#im_order.qty,
    total = Order#im_order.total,
    currency = Currency
  },
  case lists:filter(fun(#'StatisticEntity'{id = Id1}) -> Id =:= Id1 end, Acc) of
    [] ->
      [#'StatisticEntity'{
        id = Id,
        name = Name,
        total = Order#im_order.total,
        qty = Order#im_order.qty,
        currency = Currency,
        items = [Entity]
      }|Acc];
    [#'StatisticEntity'{}] ->
      lists:foldl(fun(Item = #'StatisticEntity'{id=Id2}, Acc1) ->
        case Id2 of
          Id ->
            [Item#'StatisticEntity'{
              items = [Entity | Item#'StatisticEntity'.items],
              total = Item#'StatisticEntity'.total + Order#im_order.total,
              qty = Item#'StatisticEntity'.qty + Order#im_order.qty
            }|Acc1];
          _ ->
            [Item|Acc1]
        end
      end, [], Acc)
  end.

format_mongodb_day_week(1) -> "Sunday";
format_mongodb_day_week(2) -> "Monday";
format_mongodb_day_week(3) -> "Tuesday";
format_mongodb_day_week(4) -> "Wednesday";
format_mongodb_day_week(5) -> "Thursday";
format_mongodb_day_week(6) -> "Friday";
format_mongodb_day_week(7) -> "Saturday".
