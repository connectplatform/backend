-module(im_feed_post).
-include("im_common.hrl").
-export([get/2, get_by_location/2, create/2, update/2, delete/2, get_tags/2]).
-export([get_categories/2, create_category/2, update_category/2, delete_category/2]).

get(#'FeedPost'{ref=Ref, type=Type, categories=Categories, tags=Tags, postIds=PostIds, parentId=ParentId, likedOnly=LikedOnly,
  position=Position, location=Location, distance=Distance, skip=Skip, limit=Limit, vendorsOnly=VendorsOnly, vendorId=VendorId}, UserId) ->
  User = im_roster_chat:get(UserId),
  Limit2 = case Limit =:= undefined orelse Limit =:= 0 of true -> 30; _ -> Limit end,
  Skip2 = case Skip of undefined -> 0; _ -> Skip end,

  {Selector, Skip1, Limit1} = case PostIds =:= [] orelse PostIds =:= undefined of
    true ->
      SelectorBase = case Type of
        undefined -> [];
        _         -> [<<"type">>, im_common:format_utf8(Type)]
      end,
      case LikedOnly =:= true of
        true ->
          Likes = ctail_mongo:find(
            im_like,
            list_to_tuple([<<"userId">>, UserId, <<"recordType">>,<<"feed_post">>] ++ SelectorBase),
            Skip2,
            Limit2
          ),
          PostIds1 = [Like#im_like.recordId || Like <- Likes],
          {{<<"_id">>, {<<"$in">>, PostIds1}}, 0, Limit2};
        false ->
          Selector2 = case Categories of
            undefined -> list_to_tuple(SelectorBase);
            []        -> list_to_tuple(SelectorBase);
            _         -> list_to_tuple(SelectorBase ++ [<<"categories">>, {<<"$in">>, [im_common:parse_id(Category) || Category <- Categories]}])
          end,
          Selector1 = case Tags of
            undefined -> Selector2;
            [] -> Selector2;
            _  -> list_to_tuple(tuple_to_list(Selector2) ++ [<<"tags">>, {<<"$all">>, im_common:format_utf8_array(Tags)}])
          end,
          Selector1_1 = case VendorId of
            undefined -> Selector1;
            _         -> list_to_tuple(tuple_to_list(Selector1) ++ [<<"vendorId">>, im_common:parse_id(VendorId)])
          end,
          Selector3 = case ParentId of
            undefined -> Selector1_1;
            _ -> list_to_tuple(tuple_to_list(Selector1_1) ++ [<<"parentId">>, im_common:parse_id(ParentId)])
          end,
          Selector4 = case is_list(Position) andalso length(Position) =:= 2 of
            false -> Selector3;
            true  ->
              [PositionLat, PositionLng] = Position,
              FPositionLat = im_geo_utils:filter_coor(PositionLat),
              FPositionLng = im_geo_utils:filter_coor(PositionLng),
              Distance1 = case Distance =:= undefined orelse Distance =:= 0 of
                true -> 20000000;
                false -> Distance
              end,
              list_to_tuple(tuple_to_list(Selector3) ++ [
                <<"location">>, {
                  <<"$near">>, {
                    <<"$geometry">>, {
                      <<"type">>, <<"Point">>,
                      <<"coordinates">>, [FPositionLng, FPositionLat]
                    },
                    <<"$maxDistance">>, im_geo_utils:filter_distance(Distance1)
                  }
                }
              ])
          end,
          {Selector4, Skip2, Limit2}
      end;
    false ->
      {{<<"_id">>, {<<"$in">>, [im_common:parse_id(PostId) || PostId <- PostIds]}}, Skip2, Limit2}
  end,

  Selector5 = case VendorId =:= undefined andalso VendorsOnly =:= true of
    true ->
      CurrentVendorId = case User#im_usr.isVendor =:= true of
        true -> User#im_usr.id;
        false -> User#im_usr.vendorId
      end,
      case im_acl:has_role("vendor", UserId) orelse im_acl:has_role("vendor_staff", UserId) of
        true -> list_to_tuple(tuple_to_list(Selector) ++ [<<"vendorId">>, CurrentVendorId]);
        false -> Selector
      end;
    false ->
      Selector
  end,

  Total = ctail_mongo:exec(count, [<<"im_feed_post">>, Selector5]),
  Posts = ctail_mongo:find(im_feed_post, Selector5, Skip1, Limit1),

  PostEntities = case is_list(Location) andalso length(Location) =:= 2 of
    false -> [format_post(Post, UserId) || Post <- Posts];
    true ->
      [FLocationLat, FLocationLng] = Location,
      format_and_sort_by_range_from_user(Posts, {FLocationLat, FLocationLng}, UserId)
  end,
  #'FeedPostResp'{ref=Ref, posts=PostEntities, total=Total}.

create(#'FeedPostCreate'{ref=Ref, post=PostEntity}, UserId) ->
  Post = im_dto:parse_feed_post(PostEntity),
  Now = sm:now(),
  User = im_roster_chat:get(UserId),
  VendorId = case im_acl:has_role("super_admin", UserId) of
    true  ->
      case PostEntity#'FeedPostEntity'.vendorId =:= undefined of
        true  -> UserId;
        false -> im_common:parse_id(PostEntity#'FeedPostEntity'.vendorId)
      end;
    false ->
      case User#im_usr.isVendor =:= true of
        true  -> UserId;
        false -> User#im_usr.vendorId
      end
  end,

  case VendorId of
    undefined ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED};
    _ ->
      Post1 = Post#im_feed_post{id=ctail:next_id(),
      vendorId=VendorId,
      created=Now,
      updated=Now},
      ok = ctail:put(Post1),
      upsert_tags(Post#im_feed_post.tags, Post#im_feed_post.type),
      #'FeedPostCreateResp'{ref=Ref, post=format_post(Post1, UserId)}
  end.

update(#'FeedPostUpdate'{ref=Ref, post=PostEntity}, UserId) ->
  {ok, Post} = ctail:get(im_feed_post, im_common:parse_id(PostEntity#'FeedPostEntity'.id)),
  User = im_roster_chat:get(UserId),
  PostParsed = im_dto:parse_feed_post(PostEntity),
  case can_modify_post(Post, User) of
    true ->
      NewVendorId = case im_acl:has_role("super_admin", UserId) of
        true -> PostParsed#im_feed_post.vendorId;
        false -> Post#im_feed_post.vendorId
      end,
      UpdatedFeedPost = Post#im_feed_post{
        updated=sm:now(),
        title=PostParsed#im_feed_post.title,
        descr=PostParsed#im_feed_post.descr,
        address=PostParsed#im_feed_post.address,
        parentId=PostParsed#im_feed_post.parentId,
        categories=PostParsed#im_feed_post.categories,
        tags=PostParsed#im_feed_post.tags,
        payload=PostParsed#im_feed_post.payload,
        author=PostParsed#im_feed_post.author,
        thumbnail=PostParsed#im_feed_post.thumbnail,
        buttonCaption=PostParsed#im_feed_post.buttonCaption,
        targetLink=PostParsed#im_feed_post.targetLink,
        media=PostParsed#im_feed_post.media,
        userId=PostParsed#im_feed_post.userId,
        workHours=PostParsed#im_feed_post.workHours,
        vendorId=NewVendorId,
        paymentInfo=PostParsed#im_feed_post.paymentInfo,
        location=PostParsed#im_feed_post.location
      },
      ctail:put(UpdatedFeedPost),
      upsert_tags(PostParsed#im_feed_post.tags, PostParsed#im_feed_post.type),
      #'FeedPostUpdateResp'{ref = Ref, post=format_post(UpdatedFeedPost, UserId)};
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

can_modify_post(Post, User) ->
  VendorId = case User#im_usr.isVendor =:= true of
    true -> User#im_usr.id;
    false -> User#im_usr.vendorId
  end,
  IsGrantedVendor = (im_acl:has_role("vendor", User#im_usr.id) orelse im_acl:has_role("vendor_staff", User#im_usr.id))
    andalso Post#im_feed_post.vendorId =:= VendorId,
  im_acl:has_role("super_admin", User#im_usr.id) orelse IsGrantedVendor.

upsert_tags(undefined, _) -> skip;
upsert_tags([], _) -> skip;
upsert_tags([Tag|T], Type) ->
  case ctail:get(im_feed_post_tag, Tag) of
    {ok, Tag} ->
      Types = im_common:list_unique(Tag#im_feed_post_tag.types ++ [im_common:format_utf8(Type)]),
      ctail:put(Tag#im_feed_post_tag{types=Types});
    _ ->
      ctail:put(#im_feed_post_tag{id=im_common:format_utf8(Tag), types=[im_common:format_utf8(Type)]})
  end,
  upsert_tags(T, Type).

delete(#'FeedPostDelete'{ref=Ref, id=Id}, UserId) ->
  {ok, Post} = ctail:get(im_feed_post, im_common:parse_id(Id)),
  User = im_roster_chat:get(UserId),
  case can_modify_post(Post, User) of
    true ->
      ctail:delete(im_feed_post, im_common:parse_id(Id)),
      #'FeedPostDeleteResp'{ref=Ref};
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

get_tags(#'FeedPostTag'{ref=Ref, type=Type}, _UserId) ->
  Tags = ctail_mongo:find(im_feed_post_tag, {<<"types">>, im_common:format_utf8(Type)}, 0, 999999),
  TagsFormatted = [im_common:format_utf8(Tag#im_feed_post_tag.id) || Tag <- Tags],
  #'FeedPostTagResp'{ref=Ref, tags=TagsFormatted}.

get_categories(#'FeedPostCategory'{ref=Ref, tag=Tag}, _UserId) ->
  Selector = case Tag =:= undefined of
    true -> {};
    false -> {<<"tag">>, im_common:format_utf8(Tag)}
  end,
  Categories = ctail_mongo:find(im_feed_post_category, Selector, infinity),
  CategoryEntities = [im_dto:format_feed_post_category(Category1) || Category1 <- Categories],
  #'FeedPostCategoryResp'{ref=Ref, categories=CategoryEntities}.

create_category(#'FeedPostCategoryCreate'{ref=Ref, category=CategoryEntity}, UserId) ->
  case im_acl:has_role("super_admin", UserId) of
    true ->
      Now = sm:now(),
      % IsFeatured = CategoryEntity#'FeedPostCategoryEntity'.isFeatured,
      % case IsFeatured of
      %   true ->
      %     case ctail_mongo:find(im_feed_post_category, {<<"isFeatured">>, <<"true">>}, infinity) of
      %       [] -> skip;
      %       FeaturedCategories -> [ctail:put(FeaturedCategory#im_feed_post_category{isFeatured=false}) || FeaturedCategory <- FeaturedCategories]
      %     end;
      %   false -> skip
      % end,
      Category = im_dto:parse_feed_post_category(CategoryEntity),
      Category1 = Category#im_feed_post_category{id=ctail:next_id(),
        created=Now,
        updated=Now},
      ok = ctail:put(Category1),
      #'FeedPostCategoryCreateResp'{ref=Ref, category=im_dto:format_feed_post_category(Category1)};
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

update_category(#'FeedPostCategoryUpdate'{ref=Ref, category=CategoryEntity}, UserId) ->
  case im_acl:has_role("super_admin", UserId) of
    true ->
      Category = im_dto:parse_feed_post_category(CategoryEntity),
      ok = ctail:put(Category),
      #'FeedPostCategoryUpdateResp'{ref=Ref, category=im_dto:format_feed_post_category(Category)};
    false ->
        #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

delete_category(#'FeedPostCategoryDelete'{ref=Ref, id=Id}, UserId) ->
  case im_acl:has_role("super_admin", UserId) of
    true ->
      ctail:delete(im_feed_post_category, im_common:parse_id(Id)),
      #'FeedPostCategoryDeleteResp'{ref=Ref};
    false ->
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_PERMISSION_DENIED}
  end.

get_by_location(#'FeedPostByLocation'{ref=Ref, position=[PositionLat, PositionLng], location=[LocationLat, LocationLng],
  distance=Distance, limit=Limit}, UserId) ->
  FPositionLat = im_geo_utils:filter_coor(PositionLat),
  FPositionLng = im_geo_utils:filter_coor(PositionLng),
  FLocationLat = im_geo_utils:filter_coor(LocationLat),
  FLocationLng = im_geo_utils:filter_coor(LocationLng),
  Distance1 = case Distance of
                undefined -> 50000;
                _         -> Distance
              end,
  Selector = {
    <<"$near">>, {
      <<"$geometry">>, {
        <<"type">>, <<"Point">>,
        <<"coordinates">>, [FPositionLng, FPositionLat]
      },
      <<"$maxDistance">>, im_geo_utils:filter_distance(Distance1)
    }
  },
  Total = ctail_mongo:exec(count, [<<"im_feed_post">>, {<<"location">>, Selector}]),
  FeedPosts = ctail_mongo:find(im_feed_post, {<<"location">>, Selector}, im_geo_utils:filter_limit(Limit)),
  FeedPostEntities = format_and_sort_by_range_from_user(FeedPosts, {FLocationLat, FLocationLng}, UserId),
  #'FeedPostByLocationResp'{ref=Ref, total=Total, posts=FeedPostEntities}.

format_and_sort_by_range_from_user(FeedPosts, {UserLat, UserLng}, UserId) ->
  FormatFun = fun(Post=#im_feed_post{}) ->
    PostEntity = format_post(Post, UserId),
    [FeedPostLat, FeedPostLng] = PostEntity#'FeedPostEntity'.location,
    Distance = im_geo_utils:calc_distance({FeedPostLat, FeedPostLng}, {UserLat, UserLng}),
    PostEntity#'FeedPostEntity'{distance=Distance}
  end,
  FeedPostEntities = [FormatFun(V) || V <- FeedPosts],
  lists:sort(fun(A, B) -> A#'FeedPostEntity'.distance < B#'FeedPostEntity'.distance end, FeedPostEntities).

format_post(Post, UserId) ->
  Likes = im_like:count("feed_post", Post#im_feed_post.id),
  IsLiked = im_like:is_liked("feed_post", Post#im_feed_post.id, UserId), %% todo: optimize
  PostFormatted = im_dto:format_feed_post(Post),
  PostFormatted#'FeedPostEntity'{likes=Likes, isLiked=IsLiked}.

%% Added by AI:

PostEntities = [FormatFun(FeedPost) || FeedPost <- FeedPosts],
SortedFeedPostEntities = lists:sort(fun (A, B) ->
A#'FeedPostEntity'.distance < B#'FeedPostEntity'.distance
end, FeedPostEntities),
SortedFeedPostEntities.

format_post(Post=#im_feed_post{}, UserId) ->
IsLiked = case ctail:get(im_like, {<<"userId">>, UserId, <<"recordType">>, <<"feed_post">>, <<"recordId">>, Post#im_feed_post.id}) of
{ok, _} -> true;
_ -> false
end,
im_dto:format_feed_post(Post, IsLiked).

