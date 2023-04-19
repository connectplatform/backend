-module(im_retrieve).

-include("im_common.hrl").

-export([get_messages/7]).

get_messages(UserId, FeedType, FeedId, Count, Top, Stop, Direction) ->
  Direction1 = case Direction of
    ?RETRIEVE_DIRECTION_UP   -> ?RETRIEVE_DIRECTION_UP;
    ?RETRIEVE_DIRECTION_DOWN -> ?RETRIEVE_DIRECTION_DOWN;
    _                        -> ?RETRIEVE_DIRECTION_UP %% TODO remove this backward compatibility
  end,

  FeedId1 = im_common:parse_id(FeedId),
  Top1 = im_common:parse_id(Top),
  Stop1 = im_common:parse_id(Stop),
  Count1 = case Count =:= undefined of
    true -> 100;
    false -> Count
  end,

  Messages = case FeedType of
    ?MESSAGE_FEED_TYPE_CHAT ->
      FilterFun = fun(ResultToFilter) ->
        case ResultToFilter of
          {ok, MsgToCheck} -> im_message:is_visible(UserId, MsgToCheck);
          _ -> false
        end
      end,
      im_roster_chat:retrieve(UserId, FeedId1, Top1, Stop1, Direction1, Count1, FilterFun);
    ?MESSAGE_FEED_TYPE_ROOM ->
      RoomItem = im_roster_muc:get(FeedId1),
      case lists:member(UserId, RoomItem#im_grp.members) =:= true of
        true ->
          TimeMarker = im_room:get_marker_time(UserId, FeedId1),
          io:format("TimeMarker: ~p~n", [TimeMarker]),
          FilterFun = fun(ResultToFilter) ->
            case ResultToFilter of
              {ok, MsgToCheck} -> im_message:is_visible(UserId, MsgToCheck) andalso MsgToCheck#im_msg.created >= TimeMarker;
              _ -> false
            end
          end,
          im_roster_muc:retrieve(FeedId1, UserId, Top1, Stop1, Direction1, Count1, FilterFun);
        false ->
          {error, not_a_member}
      end;
    _ -> []
  end,

  case Messages of
    {error, not_a_member} -> {error, not_a_member};
    _ -> {ok, Messages}
  end.
