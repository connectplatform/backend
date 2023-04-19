-module(im_circle).

-include("im_common.hrl").

-export([get/2]).

get(#'GetCircle'{ref=Ref, supervisor=Supervisor, params=ParamString, members=Members, forceCreate=ForceCreate}, UserId) ->
  Params = [{<<"supervisor">>, im_common:format_utf8(Supervisor)}] ++ parse_params(ParamString),


  ExistingCircles = im_roster_chat:execute(UserId, fun(User) ->
    User1 = im_chatupdate:parse(User),
    {filter_circles(User1, Params), User}
  end),

  im_logger:info(UserId, "[Cicle] Get with params: ~p, existing circles: ~p", [Params, ExistingCircles]),

  DoCreateNew = ForceCreate =:= true orelse ExistingCircles =:= [],

  Members1 = case Members =:= undefined of
    true -> [];
    false -> [im_common:parse_id(MemberId) || MemberId <- Members]
  end,

  FeedId = case DoCreateNew of
    true ->
      {ok, BotId} = im_bot:lookup_by_username(im_common:format_utf8(Supervisor)),
      {ok, Bot} = ctail:get(im_bot, BotId),

      Topic = proplists:get_value(<<"topic">>, Params),
      Picture = proplists:get_value(<<"picture">>, Params),
      Thumbnail = proplists:get_value(<<"thumbnail">>, Params),

      Topic1 = case Topic =:= undefined of
        true -> <<"New group">>;
        false -> Topic
      end,

      Params1 = lists:map(fun
        ({<<$*,ParamName/binary>>, ParamValue}) -> {ParamName, ParamValue};
        ({ParamName, ParamValue})               -> {ParamName, ParamValue}
      end, Params),

      Room = #im_grp{topic=Topic1,
        picture=im_common:format_utf8(Picture),
        thumbnail=im_common:format_utf8(Thumbnail),
        members=im_common:list_unique(Members1 ++ [Bot#im_bot.userId]),
        admins=[Bot#im_bot.userId],
        circleParams=format_params(Params1)},
      Room1 = im_room:new(Room, UserId),

      case proplists:get_value(<<"initMsg">>, Params) of
        undefined -> skip;
        InitMsg ->
          im_message:send(Bot#im_bot.userId, ?MESSAGE_FEED_TYPE_ROOM, Room1#im_grp.id, #im_msg{
            type = ?MESSAGE_TYPE_USER_MESSAGE,
            created = sm:now(),
            kind = ?MESSAGE_KIND_TEXT,
            payload = InitMsg
          })
      end,

      Room1#im_grp.id;
    false ->
      {ok, BotId} = im_bot:lookup_by_username(im_common:format_utf8(Supervisor)),
      [{UpdateId, _}|_] = ExistingCircles,
      {_, FeedId1} = im_chatupdate:roster_to_msg_feed(UpdateId, UserId),

      Room = im_roster_muc:get(FeedId1),
      Topic = proplists:get_value(<<"topic">>, Params, Room#im_grp.topic),
      Picture = proplists:get_value(<<"picture">>, Params, Room#im_grp.picture),
      Thumbnail = proplists:get_value(<<"thumbnail">>, Params, Room#im_grp.thumbnail),
      NewMembers = im_common:list_unique(Room#im_grp.members ++ Members1),

      CurParams = parse_params(Room#im_grp.circleParams),
      NewParams = lists:foldl(fun
        ({<<$*,Key/binary>>, Value}, Acc) -> lists:keystore(Key, 1, Acc, {Key, Value});
        ({Key, Value}, Acc)               -> lists:keystore(Key, 1, Acc, {Key, Value})
      end, CurParams, Params),

      NewRoom = Room#im_grp{topic=im_common:format_utf8(Topic),
        picture=im_common:format_utf8(Picture),
        thumbnail=im_common:format_utf8(Thumbnail),
        circleParams=format_params(NewParams),
        members=NewMembers},
      im_roster_muc:execute(FeedId1, fun(_Room) ->
        ctail:put(NewRoom),
        {ok, NewRoom}
      end),

      FeedId1
  end,

  im_bot_update:send_circle_update(BotId, UserId, FeedId, DoCreateNew),

  Update = im_dto:format_chat_update(UserId, im_chatupdate:wait_update(UserId, FeedId)),

  #'GetCircleResp'{ref=Ref, roomId=im_common:format_id(FeedId), update=Update}.

filter_circles(User, Params) ->
  UniqueParams = lists:filter(
    fun({<<$*,_/binary>>, _}) -> true;
      ({_, _}) -> false
    end,
    Params),

  case UniqueParams =:= [] of
    true -> [];
    false ->
      lists:filter(fun ({UpdateId, #im_chat_update{}}) ->
        {FeedType, FeedId} = im_chatupdate:roster_to_msg_feed(UpdateId, User#im_usr.id),
        case FeedType =:= ?MESSAGE_FEED_TYPE_ROOM of
          true ->
            Room = try im_roster_muc:get(FeedId) catch _:_ -> undefined end,

            case FeedId =/= undefined andalso Room =/= undefined of
              true ->
                CircleParams = parse_params(Room#im_grp.circleParams),

                MatchingParams = lists:filter(
                  fun({<<$*,ParamName/binary>>, ParamValue}) ->
                    ParamValue =:= proplists:get_value(ParamName, CircleParams)
                  end,
                  UniqueParams),

                im_logger:info(User#im_usr.id, "[Cicle] Filter step. Matching params: ~p, unique params: ~p", [MatchingParams, UniqueParams]),

                length(MatchingParams) =:= length(UniqueParams);
              false -> false
            end;
          false -> false
        end
      end, User#im_usr.chatUpdates)
  end.

parse_params(undefined) -> [];
parse_params(ParamString) ->
  Params = cow_qs:parse_qs(im_common:format_utf8(ParamString)),
  sort_params(Params).

format_params(undefined) -> [];
format_params(Params) ->
  cow_qs:qs(sort_params(Params)).

sort_params(Params) ->
  lists:sort(fun ({KeyA, _}, {KeyB, _}) ->
    im_common:ensure_list(KeyA) < im_common:ensure_list(KeyB)
  end, Params).
