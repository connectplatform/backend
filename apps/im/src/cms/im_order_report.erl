-module(im_order_report).

-include("im_common.hrl").

-export([notify_order_purchased/2]).
-export([notify_statistics_of_the_week_for_vendor/2]).

%% Api

notify_order_purchased(Order = #im_order{vendorId = VendorId, userId = UserId}, Product = #im_feed_post{}) ->
  Fun = fun() ->
    TemplatePath = filename:dirname(filename:dirname(code:which(?MODULE))) ++ "/priv/emails/order-purchased/template.html",
    User = im_roster_chat:get(UserId),
    Locale = im_common:ensure_list(im_locale:get(UserId)),
    ConnectEmail = sm:env(im, report_email),
    To = case VendorId of
      undefined -> ConnectEmail;
      _ ->
        case im_roster_chat:get(VendorId) of
          undefined -> ConnectEmail;
          Vendor -> Vendor#im_usr.email
        end
    end,

    Subject = im_common:ensure_binary(im_trans:t(Locale, <<"email.order.purchased.title">>, [Product#im_feed_post.title])),
    {<<"paymentInfo">>, PaymentInfoProps} = Product#im_feed_post.paymentInfo,

    Variables = [
      {mainText1, "New order!"},
      {footerText1, im_common:format_utf8("If you didn't create an account using this email address, please ignore this email or ")},
      {unsubscribeLink, im_common:format_utf8("http://google.com")},
      {unsubscribeText, "unsubscribe"},
      {orderNumberText, "Order number"},
      {orderNumber, im_common:format_id(Order#im_order.id)},
      {productName, Product#im_feed_post.title},
      {userName, User#im_usr.name},
      {userPhone, User#im_usr.phone},
      {userEmail, User#im_usr.email},
      {purchaseDate, Order#im_order.purchaseDate},
      {productPrice, im_dto:format_price(Order#im_order.price, proplists:get_value(<<"currency">>, PaymentInfoProps))},
      {totalPrice, im_dto:format_price(Order#im_order.total, proplists:get_value(<<"currency">>, PaymentInfoProps))},
      {orderQuantity, Order#im_order.qty}
    ],

    erlydtl:compile(TemplatePath, order_purchased),

    case order_purchased:render(Variables) of
      {ok, Template} ->
        send_notification_email(Subject, list_to_binary(Template), To);
      Error ->
        %% TODO log error
        io:format("Error render ~p", [Error]),
        Error
    end
  end,

  try Fun() of
    _ -> ok
  catch
    error:Error ->
      %% TODO log error
      io:format("Error ~p", [Error])
  end.

notify_statistics_of_the_week_for_vendor(_, []) ->
  skip;
notify_statistics_of_the_week_for_vendor(VendorId, StatisticsGroupedByProductByDay) ->
  [{WeeklyTotal, WeeklyQty}] = lists:foldl(fun(#'StatisticEntity'{qty=ProductQty, total=ProductTotal}, [{Total, Qty}]) ->
    [{Total+ProductTotal, Qty+ProductQty}]
  end, [{0,0}], StatisticsGroupedByProductByDay),
  StatisticsGroupedByProductByDay1 = lists:map(fun(Item = #'StatisticEntity'{items=Items}) ->
    ItemsSorted = lists:sort(fun(#'StatisticEntity'{id=DateA}, #'StatisticEntity'{id=DateB}) ->
      im_common:datetime_to_timestamp(dh_date:parse(DateA)) > im_common:datetime_to_timestamp(dh_date:parse(DateB))
    end, Items),
    ItemsFormatted = lists:map(fun(Item1 = #'StatisticEntity'{id=Date}) ->
      Item1#'StatisticEntity'{id=dh_date:format("M d, Y", dh_date:parse(Date))}
    end, ItemsSorted),
    Item#'StatisticEntity'{items = ItemsFormatted}
  end, StatisticsGroupedByProductByDay),

  Subject = "Sales Week Statistics",
  TemplatePath = filename:dirname(filename:dirname(code:which(?MODULE))) ++ "/priv/emails/statistcs_of_the_week_for_vendor/template.html",
  erlydtl:compile(TemplatePath, statistics_of_the_week_for_vendor, [{record_info, [{'StatisticEntity', record_info(fields, 'StatisticEntity')}]}]),
  {ok, Template} = statistics_of_the_week_for_vendor:render([
    {title, Subject},
    {mainText1, Subject},
    {productsByDay, "Products by day"},
    {productLabel, "Product"},
    {productTotalLabel, "TOTAL"},
    {productQuantityLabel, "Quantity"},
    {dailyQuantityLabel, "Daily quantity"},
    {dailyTotalLabel, "Daily total"},
    {weeklyQuantityLabel, "Weekly quantity"},
    {weeklyQuantity, WeeklyQty},
    {weeklyTotalLabel ,"Weekly sum"},
    {weeklyTotal , WeeklyTotal},
    {weeklyCurrency , "EUR"},
    {data, StatisticsGroupedByProductByDay1}
  ]),

  ConnectEmail = sm:env(im, report_email),
  To = case VendorId of
    undefined -> ConnectEmail;
    _ ->
      case im_roster_chat:get(VendorId) of
        undefined -> ConnectEmail;
        Vendor ->
          case Vendor#im_usr.email of
            undefined -> ConnectEmail;
            Email -> Email
          end
      end
  end,

  send_notification_email(Subject, list_to_binary(Template), To).

%% Local

send_notification_email(Subject, Template, To) ->
  send_notification_email(Subject, Template, sm:env(im, smtp_username), To).

send_notification_email(Subject, Template, From, To) ->
  case im:is_debug() of
    true -> skip;
    false ->
      Relay = sm:env(im, smtp_relay),
      Username = sm:env(im, smtp_username),
      Password = sm:env(im, smtp_password),
      Port = sm:env(im, smtp_port),

      Email = {
        Username,
        [To],
        mimemail:encode({
          <<"text">>,
          <<"html">>,
          [
            {<<"From">>, From},
            {<<"To">>, To},
            {<<"Subject">>, im_common:ensure_binary(Subject)}
          ],
          [],
          Template
        })
      },
      Options = [
        {ssl, true},
        {no_mx_lookups, true},
        {relay, Relay},
        {port, Port},
        {username, Username},
        {password, Password},
        {auth, always}
      ],
      Result = gen_smtp_client:send(Email, Options),
      im_logger:debug(undefined, "[SMTP] Result ~p", [Result]),
      Result
  end.
