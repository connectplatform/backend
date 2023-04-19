-module(im_localized_store).
-include("im_common.hrl").
-export([get/2, create/2, update/2, delete/2]).

get(#'LocalizedStore'{ref=Ref, location=Location}, _UserId) ->
  Selector = case is_list(Location) andalso length(Location) =:= 2 of
    false -> {};
    true ->
      [LocationLat, LocationLng] = Location,
      FLocationLat = im_geo_utils:filter_coor(LocationLat),
      FLocationLng = im_geo_utils:filter_coor(LocationLng),
      {<<"location">>, {
        <<"$near">>, {
          <<"$geometry">>, {
            <<"type">>, <<"Point">>,
            <<"coordinates">>, [FLocationLng, FLocationLat]
          },
          <<"$maxDistance">>, 999999999
        }
      }}
  end,
  Stores = ctail_mongo:find(im_localized_store, Selector, 0, 999999),
  StoreEntities = case is_list(Location) andalso length(Location) =:= 2 of
    false -> [im_dto:format_localized_store(Store) || Store <- Stores];
    true ->
      Coords = {im_geo_utils:filter_coor(lists:nth(1, Location)), im_geo_utils:filter_coor(lists:nth(2, Location))},
      format_and_sort_by_range_from_user(Stores, Coords)
  end,
  #'LocalizedStoreResp'{ref=Ref, stores=StoreEntities}.

create(#'LocalizedStoreCreate'{ref=Ref, store=Store}, _UserId) ->
  StoreParsed = im_dto:parse_localized_store(Store),
  StoreParsed1 = StoreParsed#im_localized_store{id=ctail:next_id()},
  ok = ctail:put(StoreParsed1),
  #'LocalizedStoreCreateResp'{ref=Ref, store=im_dto:format_localized_store(StoreParsed1)}.

update(#'LocalizedStoreUpdate'{ref=Ref, store=Store}, _UserId) ->
  StoreParsed = im_dto:parse_localized_store(Store),
  ok = ctail:put(StoreParsed),
  #'LocalizedStoreUpdateResp'{ref=Ref, store=im_dto:format_localized_store(StoreParsed)}.

delete(#'LocalizedStoreDelete'{ref=Ref, id=StoreId}, _UserId) ->
  ctail:delete(im_localized_store, im_common:parse_id(StoreId)),
  #'LocalizedStoreDeleteResp'{ref=Ref}.

format_and_sort_by_range_from_user(Stores, {UserLat, UserLng}) ->
  FormatFun = fun(Store) ->
    StoreEntity = im_dto:format_localized_store(Store),
    [Lat, Lng] = StoreEntity#'LocalizedStoreEntity'.location,
    Distance = im_geo_utils:calc_distance({Lat, Lng}, {UserLat, UserLng}),
    StoreEntity#'LocalizedStoreEntity'{distance=Distance}
  end,
  StoreEntities = [FormatFun(V) || V <- Stores],
  lists:sort(fun(A, B) -> A#'LocalizedStoreEntity'.distance < B#'LocalizedStoreEntity'.distance end, StoreEntities).
