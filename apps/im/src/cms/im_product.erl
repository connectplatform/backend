-module(im_product).
-include("im_common.hrl").
-export([create_order/2, get_orders/2, change_order/2, charge_order/2, use_order/2, get_order/2, delete_order/2]).

create_order(Msg = #'CreateOrder'{}, UId) ->
  im_transaction:execute({feed_post, im_common:parse_id(Msg#'CreateOrder'.productId)}, fun(#'CreateOrder'{
    ref=Ref, productId=ProductId, qty=Qty, date=Date,forceNoMerge=_ForceNoMerge,isBuyNow=IsBuyNow}, UserId) ->
    Now = sm:now(),
    ProductId1 = im_common:parse_id(ProductId),
    {ok, Product} = ctail:get(im_feed_post, ProductId1),
    {<<"paymentInfo">>, PaymentInfoProps} = Product#im_feed_post.paymentInfo,
    Type = proplists:get_value(<<"type">>, PaymentInfoProps),
    Price = proplists:get_value(<<"price">>, PaymentInfoProps, 0),
    UseQty = proplists:get_value(<<"useQty">>, PaymentInfoProps, 1),
    AvailableDates = proplists:get_value(<<"availableDates">>, PaymentInfoProps),
    AvailableUseQty = Qty * UseQty,
    Status = case IsBuyNow =:= false orelse IsBuyNow =:= undefined of
      true -> ?ORDER_STATUS_PENDING;
      false -> ?ORDER_STATUS_BUY_NOW
    end,

    case validate_available_date(Type, AvailableDates, Qty, Date) of
      ok ->
        case find_order(UserId, Product, Status, #im_order{date=Date}) of
          undefined ->
            Total = Price * Qty,
            Order = #im_order{
              id=ctail:next_id(),
              userId=UserId,
              productId=ProductId1,
              vendorId=Product#im_feed_post.vendorId,
              qty=Qty,
              type=Type,
              availableUseQty=AvailableUseQty,
              price=Price,
              total=Total,
              status=Status,
              date=Date,
              created=Now,
              updated=Now},
            ctail:put(Order),
            im_user_state:broadcast(UserId, [UserId], #'OrderChanged'{order=im_dto:format_order(Order)}, true),
            #'CreateOrderResp'{ref=Ref, order=im_dto:format_order(Order)};
          ExistingOrder ->
            CurrentQty = ExistingOrder#im_order.qty,
            NewQty = CurrentQty + Qty,
            case validate_available_date(Type, AvailableDates, NewQty, Date) of
              ok ->
                Total = Price * NewQty,
                Order = ExistingOrder#im_order{qty=NewQty,
                  availableUseQty=ExistingOrder#im_order.availableUseQty + AvailableUseQty,
                  total=Total},
                ok = ctail:put(Order),
                im_user_state:broadcast(UserId, [UserId], #'OrderChanged'{order=im_dto:format_order(Order)}, true),
                #'CreateOrderResp'{ref=Ref, order=im_dto:format_order(Order)};
              {error, Code} ->
                #'ErrorResp'{ref=Ref, code=Code}
            end
        end;
      {error, Code} ->
        #'ErrorResp'{ref=Ref, code=Code}
    end
  end, [Msg, UId]).

validate_available_date(?FEED_POST_PAYMENT_TYPE_PRODUCT_WITH_DATE, AvailableDates, Qty, Date) ->
  validate_available_date(?FEED_POST_PAYMENT_TYPE_TICKET_WITH_DATE, AvailableDates, Qty, Date);
validate_available_date(?FEED_POST_PAYMENT_TYPE_TICKET_WITH_DATE, AvailableDates, Qty, Date) ->
  case Date < sm:now() of
    true ->
      {error, ?ERROR_CODE_INVALID_DATE};
    false ->
      case AvailableDates =:= [] orelse AvailableDates =:= undefined of
        true -> ok;
        false ->
          Match = lists:filter(fun({<<"paymentInfoDate">>, DateProps}) ->
            Date1 = proplists:get_value(<<"date">>, DateProps),
            Date =:= Date1
          end, AvailableDates),
          % im_logger:debug(UserId, "AVAILABLE_DATES: ~p", [AvailableDates]),
          % im_logger:debug(UserId, "MATCH: ~p", [Match]),
          case Match of
            []                                   -> {error, ?ERROR_CODE_INVALID_DATE};
            [{<<"paymentInfoDate">>, DateProps}] ->
              AvailableQty = proplists:get_value(<<"availableQty">>, DateProps),
              AvailableToBuyQty = proplists:get_value(<<"availableToBuyQty">>, DateProps),
              % im_logger:debug(UserId, "AV_QTY: ~p, AV_TO_BUY_QTY: ~p, AVAILABLE: ~p", [AvailableQty, AvailableToBuyQty, AvailableQty =:= -1 orelse AvailableToBuyQty >= Qty]),
              case AvailableQty =:= -1 orelse AvailableToBuyQty >= Qty of
                true -> ok;
                false -> {error, ?ERROR_CODE_QTY_NOT_AVAILABLE}
              end
          end
      end
  end;
validate_available_date(_, _, _, _) -> ok.

reserve_for_available_date(Product, Date, Qty) ->
  {<<"paymentInfo">>, PaymentInfoProps} = Product#im_feed_post.paymentInfo,
  case proplists:get_value(<<"type">>, PaymentInfoProps) of
    ?FEED_POST_PAYMENT_TYPE_TICKET_WITH_DATE ->
      AvailableDates = lists:map(fun(AvailableDate = {<<"paymentInfoDate">>, DateProps}) ->
        Date1 = proplists:get_value(<<"date">>, DateProps),
        case Date =:= Date1 of
          false -> AvailableDate;
          true ->
            case proplists:get_value(<<"availableQty">>, DateProps) =:= -1 of
              true -> AvailableDate;
              false ->
                ReservedQty = proplists:get_value(<<"reservedQty">>, DateProps),
                AvailableToBuyQty = proplists:get_value(<<"availableToBuyQty">>, DateProps),
                DateProps1 = lists:keyreplace(<<"reservedQty">>, 1, DateProps, {<<"reservedQty">>, ReservedQty + Qty}),
                DateProps2 = lists:keyreplace(<<"availableToBuyQty">>, 1, DateProps1, {<<"availableToBuyQty">>, AvailableToBuyQty - Qty}),
                {<<"paymentInfoDate">>, DateProps2}
            end
        end
      end, proplists:get_value(<<"availableDates">>, PaymentInfoProps)),
      PaymentInfoProps1 = lists:keyreplace(<<"availableDates">>, 1, PaymentInfoProps, {<<"availableDates">>, AvailableDates}),
      Product1 = Product#im_feed_post{paymentInfo={<<"paymentInfo">>, PaymentInfoProps1}},
      % im_logger:debug(UserId, "reserve> PRODUCT: ~p, NEW_PRODUCT: ~p", [Product, Product1]),
      ctail:put(Product1);
    _ ->
      skip
  end.

find_order(_, _, ?ORDER_STATUS_BUY_NOW, _) -> undefined;
find_order(UserId, Product, Status, Order) ->
  % im_logger:debug(UserId, "SEARCH ORDER: ~p", [{<<"userId">>, im_common:format_id(UserId), <<"productId">>, im_common:format_id(Product#im_feed_post.id), <<"status">>, Status}]),
  OrderId = case Order of
    undefined -> undefined;
    _         -> Order#im_order.id
  end,
  case ctail_mongo:find(im_order, {<<"userId">>, UserId, <<"productId">>, Product#im_feed_post.id, <<"status">>, Status}, 0, 999) of
    [] ->
      % im_logger:debug(UserId, "ORDER FOR MERGE NOT FOUND", []),
      undefined;
    Orders ->
      % im_logger:debug(UserId, "ORDERS FOR THE MERGE: ~p", [Orders]),
      Fun = fun(Order1) ->
        ExistingOrderId = Order1#im_order.id,
        case ExistingOrderId of
            OrderId -> false;
            _       -> Order1#im_order.date =:= Order#im_order.date
        end
      end,
      case lists:filter(Fun, Orders) of
        [] -> undefined;
        [Order2|_] -> Order2
      end
  end.

get_orders(#'Orders'{ref=Ref, status=Status, limit=Limit, skip=Skip, vendorsOnly=VendorsOnly, orderIds=OrderIds}, UserId) ->
  case VendorsOnly =:= true andalso not im_acl:has_perm("manage_orders", UserId) of
    true ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED};
    false ->
      Limit1 = case Limit of undefined -> 30; _ -> Limit end,
      Skip1 = case Skip of undefined -> 0; _ -> Skip end,
      Selector2 = case OrderIds =:= [] orelse OrderIds =:= undefined of
        true ->
          Selector1 = case Status =:= 0 orelse Status =:= undefined of
            true -> {};
            false -> {<<"status">>, Status}
          end,
          case VendorsOnly =:= true of
            true ->
              User = im_roster_chat:get(UserId),
              VendorId = case User#im_usr.isVendor =:= true of
                true -> User#im_usr.id;
                false -> User#im_usr.vendorId
              end,
              list_to_tuple(tuple_to_list(Selector1) ++ [<<"vendorId">>, VendorId]);
            false ->
              list_to_tuple(tuple_to_list(Selector1) ++ [<<"userId">>, im_common:parse_id(UserId)])
          end;
        _ ->
          {<<"_id">>, {<<"$in">>, [im_common:parse_id(Id) || Id <- OrderIds]}}
      end,
      Orders = ctail_mongo:find(im_order, Selector2, Skip1, Limit1),
      OrderEntities = [im_dto:format_order(Order) || Order <- Orders],
      Total = ctail_mongo:exec(count, [<<"im_order">>, Selector2]),
      #'OrdersResp'{ref=Ref, orders=OrderEntities, total=Total}
  end.

get_order(#'Order'{ref=Ref, orderId=OrderId, serial=Serial}, _UserId) ->
  % im_logger:debug(UserId, "GET ORDER BY ID. ID: ~p, SERIAL: ~p", [OrderId, Serial]),
  Result = case Serial of
    undefined ->
      ctail:get(im_order, im_common:parse_id(OrderId));
    _ ->
      case ctail:get(im_order_serial_lookup, im_common:format_id(Serial)) of
        {ok, Lookup} -> ctail:get(im_order, Lookup#im_order_serial_lookup.orderId);
        _            -> {error, invalid_serial}
      end
  end,
  % im_logger:debug(UserId, "GET ORDER BY ID. RESULT: ~p", [Result]),
  case Result of
    {ok, Order} -> #'OrderResp'{ref=Ref, order=im_dto:format_order(Order)};
    {error, _}  -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_NOT_FOUND}
  end.

change_order(#'ChangeOrder'{ref=Ref, order=#'OrderEntity'{id=Id, qty=Qty, date=Date}}, UserId) ->
  {ok, Order} = ctail:get(im_order, im_common:parse_id(Id)),
  {ok, Product} = ctail:get(im_feed_post, Order#im_order.productId),
  {<<"paymentInfo">>, PaymentInfoProps} = Product#im_feed_post.paymentInfo,
  Type = proplists:get_value(<<"type">>, PaymentInfoProps),
  Price = proplists:get_value(<<"price">>, PaymentInfoProps, 0),
  % UseQty = proplists:get_value(<<"useQty">>, PaymentInfoProps, 1),
  AvailableDates = proplists:get_value(<<"availableDates">>, PaymentInfoProps),

  case Order#im_order.userId =:= UserId orelse im_acl:has_role("super_admin", UserId) of
    true ->
      case validate_available_date(Type, AvailableDates, Qty, Order#im_order.date) of
        ok ->
          Total = Price * Qty,
          AvailableUseQty = Qty,%% * UseQty,
          Order1 = Order#im_order{qty=Qty, date=Date, availableUseQty=AvailableUseQty, total=Total},
          ok = ctail:put(Order1),
          OrderFormatted = im_dto:format_order(Order1),
          im_user_state:broadcast(UserId, [UserId], #'OrderChanged'{order=OrderFormatted}, true),
          #'ChangeOrderResp'{ref=Ref, order=OrderFormatted};
        {error, Code} ->
          #'ErrorResp'{ref=Ref, code=Code}
      end;
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

charge_order(Msg = #'ChargeOrder'{}, UId) ->
  TransactionIds = lists:foldl(fun(OrderId, Acc) ->
    case ctail:get(im_order, im_common:parse_id(OrderId)) of
      {ok, #im_order{productId = ProductId}} -> [{feed_post, im_common:parse_id(ProductId)}|Acc];
      _ -> Acc
    end
  end, [], Msg#'ChargeOrder'.orderIds),

  im_transaction:execute_all(TransactionIds, fun(#'ChargeOrder'{ref=Ref, token=Token, orderIds=OrderIds}, UserId) ->
    case validate_and_calc_amount(OrderIds) of
      {ok, {Orders, Products, Amount}} ->
        {StatusCode, Data} = case im:is_debug() of
          true ->
            {200, "{\"test\": true}"};
          false ->
            ApiKey = "sk_test_qdA3bL9Bf2Xq59D66UsAstpD",
            URL = "https://" ++ ApiKey ++ "@api.stripe.com/v1/charges",
            Header = [],
            MimeType = "application/x-www-form-urlencoded",
            Body = string:join([
              string:concat("amount=", integer_to_list(round(Amount))),
              string:concat("currency=", "eur"),
              string:concat("source=", im_common:ensure_list(Token))
              % string:concat("metadata[order_id]=", im_common:ensure_list(OrderId))
            ], "&"),
            % im_logger:debug(UserId, "[Product] Sending request to stripe: ~p", [Body]),
            HTTPOptions = [],
            Options = [],
            Response = httpc:request(post, {URL, Header, MimeType, Body}, HTTPOptions, Options),
            {ok, {{_, StatusCode1, _}, _, Data1}} = Response,
            {StatusCode1, Data1}
        end,

        % im_logger:debug(UserId, "[Product] Got response from stripe. Status code: ~p, data: ~p", [StatusCode, Data]),

        Result = case StatusCode of
          200 ->
            CheckoutId = im_common:format_utf8(im_common:ensure_list(im_common:crypto_random_string()) ++
            im_common:ensure_list(im_common:crypto_random_string())),
            OrdersFormatted = lists:zipwith(fun (Order=#im_order{id=Id, qty=Qty}, Product) ->
              Order1 = case find_order(UserId, Product, ?ORDER_STATUS_PAID, Order) of
                undefined ->
                  % im_logger:debug(UserId, "CHARGE: NO MERGE", []),
                  Serial = im_common:format_utf8(im_common:crypto_random_string()),
                  Lookup = #im_order_serial_lookup{id=Serial, orderId=Id},
                  ok = ctail:put(Lookup),
                  Now = sm:now(),
                  Order#im_order{status=?ORDER_STATUS_PAID,
                    serial=Serial,
                    updated=Now,
                    purchaseDate=Now,
                    checkoutId=CheckoutId,
                    availableUseQty=Qty};
                ExistingOrder ->
                  ok = ctail:delete(im_order, Order#im_order.id),
                  im_user_state:broadcast(UserId, [UserId], #'OrderDeleted'{orderId=im_common:format_id(Id)}, true),
                  ExistingOrder#im_order{
                    qty=Order#im_order.qty + ExistingOrder#im_order.qty,
                    total=Order#im_order.total + ExistingOrder#im_order.total,
                    availableUseQty=Qty + ExistingOrder#im_order.availableUseQty,
                    updated=sm:now()}
              end,
              ok = ctail:put(Order1),
              reserve_for_available_date(Product, Order1#im_order.date, Qty),
              OrderFormatted = im_dto:format_order(Order1),
              im_user_state:broadcast(UserId, [UserId], #'OrderChanged'{order=OrderFormatted}, true),
              im_order_report:notify_order_purchased(Order1, Product),
              OrderFormatted
            end, Orders, Products),
            #'ChargeOrderResp'{ref=Ref, orders=OrdersFormatted};
          _ ->
            % im_logger:error(UserId, "Got error from Stripe. Status code: ~p, data: ~p", [StatusCode, Data]),
            {ok, {struct, Props}} = yaws_json2:decode_string(im_common:ensure_list(Data)),
            {struct, ErrorProps} = proplists:get_value("error", Props),
            Message = proplists:get_value("message", ErrorProps),
            #'ErrorResp'{ref=Ref, message=Message}
        end,
        Result;
      {error, Code} ->
        #'ErrorResp'{ref=Ref, code=Code}
    end
  end, [Msg, UId]).

validate_and_calc_amount(OrderIds) ->
  {Orders, Products, Amount} = lists:foldl(fun(OrderId, {Orders, Products, Amount}) ->
    {ok, Order} = ctail:get(im_order, im_common:parse_id(OrderId)),
    ProductId = im_common:parse_id(Order#im_order.productId),
    {ok, Product} = ctail:get(im_feed_post, ProductId),
    {<<"paymentInfo">>, PaymentInfoProps} = Product#im_feed_post.paymentInfo,
    Price = proplists:get_value(<<"price">>, PaymentInfoProps),
    Qty = Order#im_order.qty,
    Total = Price * Qty,
    {[Order|Orders], [Product|Products], Amount + Total}
  end, {[], [], 0}, OrderIds),

  Results = lists:zipwith(fun (#im_order{qty=Qty, date=Date}, #im_feed_post{paymentInfo={<<"paymentInfo">>, PaymentInfoProps}}) ->
    Type = proplists:get_value(<<"type">>, PaymentInfoProps),
    AvailableDates = proplists:get_value(<<"availableDates">>, PaymentInfoProps),
    validate_available_date(Type, AvailableDates, Qty, Date)
  end, Orders, Products),

  case lists:filter(fun (Result) -> Result =/= ok end, Results) of
    [] -> {ok, {Orders, Products, Amount}};
    ErrorResults  -> hd(ErrorResults) %% return first error code
  end.

use_order(#'UseOrder'{ref=Ref, serial=Serial, qty=Qty}, UserId) ->
  case ctail:get(im_order_serial_lookup, im_common:format_id(Serial)) of
    {ok, Lookup} ->
      {ok, Order} = ctail:get(im_order, Lookup#im_order_serial_lookup.orderId),
      case Order#im_order.availableUseQty >= Qty of
        true ->
          NewAvailableUseQty = Order#im_order.availableUseQty - Qty,
          Order1 = Order#im_order{availableUseQty=NewAvailableUseQty},
          Order2 = case NewAvailableUseQty of
            0 -> Order1#im_order{status=?ORDER_STATUS_ARCHIEVED};
            _ -> Order1
          end,
          ok = ctail:put(Order2),
          OrderFormatted = im_dto:format_order(Order2),
          im_user_state:broadcast(UserId, [UserId], #'OrderChanged'{order=OrderFormatted}, true),
          #'UseOrderResp'{ref=Ref, order=OrderFormatted};
        false ->
          #'ErrorResp'{ref=Ref, message="Qty is to big"}
      end;
    _ ->
      #'ErrorResp'{ref=Ref, message="Invalid serial"}
  end.

delete_order(#'DeleteOrder'{ref=Ref, orderId=OrderId}, _UserId) ->
  % {ok, Order} = ctail:get(im_order, im_common:parse_id(OrderId)),
  % {ok, Product} = ctail:get(im_feed_post, Order#im_order.productId),
  % {<<"paymentInfo">>, PaymentInfoProps} = Product#im_feed_post.paymentInfo,
  % Type = proplists:get_value(<<"type">>, PaymentInfoProps),
  % case Type of
  %   ?FEED_POST_PAYMENT_TYPE_PRODUCT_WITH_DATE ->
  %   ?FEED_POST_PAYMENT_TYPE_TICKET_WITH_DATE ->
  %     reserve_for_available_date(Product, Order#im_order.date, -Order#im_order.qty);
  %   _ -> skip
  % end,
  ctail:delete(im_order, im_common:parse_id(OrderId)),
  #'DeleteOrderResp'{ref=Ref}.
