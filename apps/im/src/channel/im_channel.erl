-module(im_channel).

-include("im_common.hrl").

-define(PAGE_SIZE, 10).

%% API
-export([get_channel_categories/2, get_channels/2]).

-export([create_channel/2, create_channel/3, create_channel/6]).
-export([publish_post/2]).
-export([get_posts/2]).
-export([my_channels/2]).
-export([like_post/2, unlike_post/2]).
-export([get_post_thread/2, add_comment_post/2]).
-export([subscribe_channel/2, unsubscribe_channel/2, get_my_channels/2]).

subscribe_channel(#'SubscribeChannel'{ref=Ref, channelId=ChannelId}, UserId) ->
  ChannelId1 = im_common:parse_id(ChannelId),
  case ctail:get(channel, ChannelId1) of
    {ok, Channel} ->
      ensure_user_channel_sub_lookup(UserId),

      {ok, ChannelUser} = ctail:get(channel_user_sub_lookup, Channel#channel.id),
      NewUsers = lists:usort([UserId|ChannelUser#channel_user_sub_lookup.users]),
      NewChannelUser = ChannelUser#channel_user_sub_lookup{users=NewUsers},
      ctail:put(NewChannelUser),

      {ok, UserChannel} = ctail:get(user_channel_sub_lookup, UserId),
      NewChannels = lists:usort([Channel#channel.id|UserChannel#user_channel_sub_lookup.channels]),
      NewUserChannel = UserChannel#user_channel_sub_lookup{channels=NewChannels},
      ctail:put(NewUserChannel);
    _             -> skip
  end,

  #'SubscribeChannelResp'{ref=Ref}.

unsubscribe_channel(#'UnsubscribeChannel'{ref=Ref, channelId=ChannelId}, UserId) ->
  ChannelId1 = im_common:parse_id(ChannelId),
  case ctail:get(channel, ChannelId1) of
    {ok, Channel} ->
      ensure_user_channel_sub_lookup(UserId),

      {ok, ChannelUser} = ctail:get(channel_user_sub_lookup, Channel#channel.id),
      NewUsers = lists:usort(lists:delete(UserId, ChannelUser#channel_user_sub_lookup.users)),
      NewChannelUser = ChannelUser#channel_user_sub_lookup{users=NewUsers},
      ctail:put(NewChannelUser),

      {ok, UserChannel} = ctail:get(user_channel_sub_lookup, UserId),
      NewChannels = lists:usort(lists:delete(Channel#channel.id, UserChannel#user_channel_sub_lookup.channels)),
      NewUserChannel = UserChannel#user_channel_sub_lookup{channels=NewChannels},
      ctail:put(NewUserChannel);
    _             -> skip
  end,

  #'UnsubscribeChannelResp'{ref=Ref}.

my_channels(#'GetMyChannels'{ref=Ref}, UserId) ->
  ensure_user_channel_lookup(UserId),
  {ok, Lookup} = ctail:get(user_channel_lookup, UserId),
  ChannelIds = Lookup#user_channel_lookup.channelIds,

  Channels = format_channels_by_ids(ChannelIds),

  #'GetMyChannelsResp'{ref=Ref, channels=Channels}.

get_my_channels(#'GetMySubscribedChannels'{ref=Ref}, UserId) ->
  ensure_user_channel_sub_lookup(UserId),
  {ok, UserChannel} = ctail:get(user_channel_sub_lookup, UserId),
  ChannelIds = UserChannel#user_channel_sub_lookup.channels,

  Channels = format_channels_by_ids(ChannelIds),

  #'GetMySubscribedChannelsResp'{ref=Ref, channels=Channels}.

get_post_thread(#'GetChannelPostThread'{ref=Ref, postId=PostId}, _UserId) ->
  PostId1=im_common:parse_id(PostId),
  {ok, Thread} = ctail:get(channel_post_thread, PostId1),
  Thread1 = [
    begin
      C1 = im_common:proplist_to_tuple(C),
      C1#'ChannelPostComment'{authorId=im_common:format_id(C1#'ChannelPostComment'.authorId)}
    end || C <- Thread#channel_post_thread.comments ],

  #'GetChannelPostThreadResp'{ref=Ref, thread=Thread1}.

add_comment_post(#'CommentChannelPost'{
  ref     = Ref,
  postId  = PostId,
  comment = Comment},
UserId) ->
  PostId1 = im_common:parse_id(PostId),

  Comment1 = Comment#'ChannelPostComment'{
    id       = list_to_binary(im_common:random_string(30, chars_and_numbers)),
    authorId = UserId,
    created  = sm:now()
  },

  Pid = im_worker_pool:ensure(im_channel_post_sup, PostId1, [PostId1]),
  ok = gen_server:call(Pid, {add_comment, PostId1, im_common:tuple_to_proplist(Comment1)}),

  #'CommentChannelPostResp'{ref = Ref}.

like_post(#'LikeChannelPost'{ref=Ref, postId=PostId}, UserId) ->
  PostId1 = im_common:parse_id(PostId),

  Pid = im_worker_pool:ensure(im_channel_post_sup, PostId1, [PostId1]),
  LikesCount = gen_server:call(Pid, {like, PostId1, UserId}),
  #'LikeChannelPostResp'{ref=Ref, count=LikesCount}.

unlike_post(#'UnLikeChannelPost'{ref=Ref, postId=PostId}, UserId) ->
  PostId1 = im_common:parse_id(PostId),

  Pid = im_worker_pool:ensure(im_channel_post_sup, PostId1, [PostId1]),
  LikesCount = gen_server:call(Pid, {unlike, PostId1, UserId}),
  #'UnLikeChannelPostResp'{ref=Ref, count=LikesCount}.

get_channel_categories(#'GetChannelCategories'{ref=Ref}, _UserId) ->
  Cats = [ im_dto:format_channel_category(Cat) || Cat <- ctail:all(channel_category) ],
  #'GetChannelCategoriesResp'{ref=Ref, categories = Cats}.

get_channels(#'GetChannels'{ref=Ref}, _UserId) ->
  Channels = lists:map(fun im_dto:format_channel/1, ctail:all(channel)),
  #'GetChannelsResp'{ref=Ref, channels=Channels}.

create_channel(#'CreateChannel'{ref=Ref, name=Name, descr=Descr, thumb=Thumb, locationId=LocationId, categoryId=CategoryId}, UserId) ->
  create_channel(Name, Descr, Thumb, im_common:ensure_binary(LocationId), im_common:ensure_binary(CategoryId), UserId),
  #'CreateChannelResp'{ref = Ref}.

create_channel(Name, Descr, Thumb) -> create_channel(Name, Descr, Thumb, undefined, undefined, undefined).
create_channel(Name, Descr, Thumb, LocationId, CategoryId, OwnerId) ->
  OwnerIdToUse = case OwnerId =:= undefined of
    true -> ?SYS_USER_ID;
    false ->
      case im_roster_chat:get(OwnerId) of
        undefined -> ?SYS_USER_ID;
        User -> User#im_usr.id
    end
  end,
  CategoryIdToUse = case CategoryId of
                     undefined -> undefined;
                     _         -> case ctail:get(channel_category, CategoryId) of
                                    {ok, Category} -> Category#channel_category.id;
                                    _              -> undefined
                                  end
                   end,
  LocationIdToUse = case LocationId of
                     undefined -> undefined;
                     _         -> case ctail:get(channel_location, LocationId) of
                                    {ok, Location} -> Location#channel_location.id;
                                    _              -> im_gmaps:submit_location(LocationId)
                                  end
                   end,
  Channel = #channel{
    id          = ctail:next_id(),
    owner_id    = OwnerIdToUse,
    location_id = LocationIdToUse,
    category_id = CategoryIdToUse,
    name        = im_common:format_utf8(Name),
    descr       = im_common:format_utf8(Descr),
    thumb       = im_common:format_utf8(Thumb),
    user_count  = 0
  },
  ok = ctail:put(Channel),
  ok = ctail:put(#channel_user_sub_lookup{id=Channel#channel.id, users=[]}),
  update_user_channel_lookup(Channel#channel.id, OwnerIdToUse),

  Channel.

publish_post(#'PublishPost'{ref=Ref, post=#'ChannelPost'{
  channelIds=ChannelIds,
  payload=Payload,
  ogData=OgData,
  mediaType=MediaType,
  media=Media
}}, UserId) ->
  User = im_roster_chat:get(UserId),
  ChannelIdsToUse = validate_channel_ids(ChannelIds),

  case length(ChannelIdsToUse) > 0 of
    true ->
      OgDataToUse = case OgData of
                      #'OgData'{} -> OgData;
                      undefined   -> #'OgData'{}
                    end,
      NewPost = #channel_post{
        id=ctail:next_id(),
        author_id=User#im_usr.id,
        author_name=User#im_usr.name,
        author_avatar=User#im_usr.thumbnail,
        created_at=sm:now(),
        payload=Payload,
        og_data=im_common:tuple_to_proplist(OgDataToUse),
        media_type=MediaType,
        media=im_common:tuple_to_proplist(Media),
        shares=im_common:tuple_to_proplist(#'Shares'{count=0}),
        likes=im_common:tuple_to_proplist(#'Likes'{count=0}),
        thread=im_common:tuple_to_proplist(#'Thread'{count=0})

      },
      ok = ctail:put(NewPost),

      ok = ctail:put(#channel_post_share_lookup{id=NewPost#channel_post.id, shares=[]}),
      ok = ctail:put(#channel_post_like_lookup{id=NewPost#channel_post.id, likes=[]}),
      ok = ctail:put(#channel_post_thread{id=NewPost#channel_post.id, comments=[]}),

      [create_post_link(feed_id(ChannelId), ChannelId, NewPost#channel_post.id) || ChannelId <- ChannelIdsToUse],
      [notify_channel_subscribers(ChannelId, NewPost#channel_post.id) || ChannelId <- ChannelIdsToUse],

      #'PublishPostResp'{ref=Ref, post=format_channel_post(NewPost, UserId)};
    _    -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_UNKNOWN}
  end.

create_post_link(FeedId, ChannelId, PostId) ->
  Link = #channel_post_link{
    id=ctail:next_id(),
    feed_id=FeedId,
    post_id=PostId,
    channel_id=ChannelId
  },
  ctail_feed:add(Link),
  ok.

notify_channel_subscribers(ChannelId, PostId) ->
  ensure_channel_user_sub_lookup(ChannelId),
  {ok, ChannelUserLookup} = ctail:get(channel_user_sub_lookup, ChannelId),
  Users = ChannelUserLookup#channel_user_sub_lookup.users,
  Update = #'ChannelUpdate'{
    channelId=im_common:format_id(ChannelId),
    postId=im_common:format_id(PostId)
  },
  im_user_state:broadcast(undefined, Users, Update).

validate_channel_ids(Ids) ->
  lists:foldl(fun(Id, Acc) ->
                ParsedId = im_common:parse_id(Id),
                case ctail:get(channel, ParsedId) of
                  {ok, #channel{}} -> [ParsedId|Acc];
                  _                -> Acc
                end
              end, [], Ids).

feed_id(FeedId) -> {channel, FeedId}.

get_posts(#'GetPosts'{ref=Ref, channelId=ChannelId, lastId=LastId}, UserId) ->
  ParsedChannelId = im_common:parse_id(ChannelId),
  ParsedLastId = im_common:parse_id(LastId),
  case ctail:get(channel, ParsedChannelId) of
    {ok, #channel{}} ->
      FeedId = feed_id(ParsedChannelId),
      StartId = case LastId of
                  undefined -> undefined;
                  _         -> ParsedLastId
                end,
      PostLinks = ctail_feed:get(channel_post_link, FeedId, StartId, undefined, ?PAGE_SIZE),
      Posts = lists:foldl(fun(PostLink, Acc) ->
                         case ctail:get(channel_post, PostLink#channel_post_link.post_id) of
                           {ok, ChannelPost} -> [ format_channel_post(ChannelPost, UserId) | Acc ];
                           _                 -> Acc
                         end
                       end, [], PostLinks),
      #'GetPostsResp'{ref=Ref, posts=Posts};
    _                -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_UNKNOWN}
  end.

format_channel_post(#channel_post{}=Post, UserId) ->
  #'ChannelPost'{
    id=im_common:format_id(Post#channel_post.id),
    authorId=im_common:format_id(Post#channel_post.author_id),
    authorName=Post#channel_post.author_name,
    authorAvatar=Post#channel_post.author_avatar,
    created=Post#channel_post.created_at,
    payload=Post#channel_post.payload,
    ogData=im_common:proplist_to_tuple(Post#channel_post.og_data),
    mediaType=Post#channel_post.media_type,
    media=im_common:proplist_to_tuple(Post#channel_post.media),
    shares=im_common:proplist_to_tuple(Post#channel_post.shares),
    likes=im_common:proplist_to_tuple(Post#channel_post.likes),
    thread=im_common:proplist_to_tuple(Post#channel_post.thread),
    liked=is_liked_post(Post#channel_post.id, UserId)
  }.

is_liked_post(PostId, UserId) ->
  {ok, Lookup} = ctail:get(channel_post_like_lookup, im_common:parse_id(PostId)),
  lists:member(im_common:parse_id(UserId), Lookup#channel_post_like_lookup.likes).

format_channels_by_ids(ChannelIds) ->
  lists:foldl(fun(Id, Acc) ->
    case ctail:get(channel, im_common:parse_id(Id)) of
      {ok, Channel} -> [im_dto:format_channel(Channel)|Acc];
      _             -> Acc
    end
              end,
    [], ChannelIds).

update_user_channel_lookup(ChannelId, UserId) ->
  ensure_user_channel_lookup(UserId),
  {ok, Lookup} = ctail:get(user_channel_lookup, UserId),
  ChannelIds = lists:usort(Lookup#user_channel_lookup.channelIds ++ [im_common:format_id(ChannelId)]),
  Lookup1 = Lookup#user_channel_lookup{channelIds=ChannelIds},
  ctail:put(Lookup1).

ensure_user_channel_lookup(Id) ->
  case ctail:get(user_channel_lookup, Id) of
    {ok, _Lookup} -> skip;
    _             -> ctail:put(#user_channel_lookup{id=Id, channelIds=[]})
  end.

ensure_user_channel_sub_lookup(Id) ->
  case ctail:get(user_channel_sub_lookup, Id) of
    {ok, _Lookup} -> skip;
    _             -> ctail:put(#user_channel_sub_lookup{id=Id, channels=[]})
  end.

ensure_channel_user_sub_lookup(Id) ->
  case ctail:get(channel_user_sub_lookup, Id) of
    {ok, _Lookup} -> skip;
    _             -> ctail:put(#channel_user_sub_lookup{id=Id, users=[]})
  end.
