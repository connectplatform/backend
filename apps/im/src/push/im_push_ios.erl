-module(im_push_ios).

-include("im_common.hrl").

-export([send_message/2, send_alert/3, send_badge/2, send_voip/2]).

send_message(UserId, Msg=#im_msg{}) ->
  % User = im_roster_chat:get(UserId),
  {_, FeedId} = im_chatupdate:roster_to_msg_feed(Msg#im_msg.feed_id, UserId),
  Alert = im_push_tools:format_message_alert(UserId, Msg),
  case Alert =/= undefined of
    true ->
      im_push:send(UserId, ?PLATFORM_IOS, [
        {<<"notification">>, [
          {<<"body">>, Alert}
        ]},
        {<<"data">>, [
          {<<"feed_id">>, im_common:format_id(FeedId)}
        ]},
        {<<"apns">>, [
          % {<<"headers">>, [
          %   {<<"apns-expiration">>, "0"}
          % ]},
          {<<"payload">>, [
            {<<"aps">>, [
              {<<"sound">>, <<"incoming.mp3">>}
              % {<<"badge">>, im_chatupdate:calculate_total_unread(User)}
            ]}
          ]}
        ]},
        {<<"collapse_key">>, <<"message">>}
      ]);
    false ->
      skip
  end.

send_alert(UserId, AlertText, CollapseKey) ->
  im_push:send(UserId, ?PLATFORM_IOS, [
    {<<"notification">>, [
      {<<"body">>, im_common:format_utf8(AlertText)}
    ]},
    {<<"collapse_key">>, im_common:format_utf8(CollapseKey)}
  ]).

send_badge(User=#im_usr{}, Badge) ->
  % im_logger:debug(UserId, "[PushIOS] Send badge notification: ~p", [Badge]),
  im_push:send(User, ?PLATFORM_IOS, [
    {<<"notification">>, [
      {<<"badge">>, Badge}
    ]}
  ]);
send_badge(UserId, Badge) ->
  send_badge(im_roster_chat:get(UserId), Badge).

send_voip(UserId, Data) ->
  im_logger:info(UserId, "[PushIOS] Send VoIP notification: ~p", [Data]),

  lists:foreach(fun(#im_device_push_token{id=Token}) ->
    im_apn_pool:send(UserId, Token, Data)
  end, im_device:filter_push_tokens(UserId, ?PLATFORM_IOS, ?PUSH_TOKEN_TYPE_VOIP)).
