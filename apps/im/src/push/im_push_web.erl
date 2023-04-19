-module(im_push_web).

-include("im_common.hrl").

-export([send_message/2, send_alert/3]).

send_message(UserId, Msg=#im_msg{}) ->
  {_FeedType, FeedId} = im_chatupdate:roster_to_msg_feed(Msg#im_msg.feed_id, UserId),

  % {_Title, Icon} = im_push_tools:get_chat_name(UserId, FeedType, FeedId),
  Body = im_push_tools:format_message_alert(UserId, Msg),

  case Body =/= undefined of
    true ->
      im_push:send(UserId, ?PLATFORM_WEB, [
        {<<"notification">>, [
          {<<"body">>, im_common:format_utf8(Body)},
          % {<<"icon">>, im_common:format_utf8(Icon)},
          {<<"click_action">>, im_common:format_utf8("https://" ++ sm:env(im, web_url))},
          {<<"tag">>, <<"message">>}
        ]},
        {<<"priority">>, <<"high">>},
        {<<"data">>, [
          {<<"feed_id">>, im_common:format_id(FeedId)}
        ]},
        {<<"collapse_key">>, <<"message">>}
      ]);
    false -> skip
  end.

send_alert(UserId, Title, CollapseKey) ->
  im_push:send(UserId, ?PLATFORM_WEB, [
    {<<"data">>, [
      {<<"title">>, im_common:format_utf8(Title)}
    ]},
    {<<"collapse_key">>, im_common:format_utf8(CollapseKey)}
  ]).
