-module(im_gmaps).

-include("im_common.hrl").

-export([suggest_location/2, location_by_gps/2]).
-export([get_locations_suggestions/1, get_langs/0, get_json/1, geocode_latlng_url/2, location_by_id/1]).
-export([calc_radius/1, calc_half_distance/1]).
-export([submit_location/1]).

-define(EARTH_RADIUS, 6372795).
-define(FALLBACK_LOCALE, <<"en">>).
-define(API_KEY, <<"AIzaSyDsLlhlbHAg3QX8ss5SK3hc54Vta-1sph4">>).

%%  Url = <<"https://maps.googleapis.com/maps/api/place/details/json?placeid=ChIJf5dkYIZL0UAR2LXLqSPn3Pg&key=AIzaSyDsLlhlbHAg3QX8ss5SK3hc54Vta-1sph4">>
%%https://maps.googleapis.com/maps/api/place/textsearch/json?key=AIzaSyDsLlhlbHAg3QX8ss5SK3hc54Vta-1sph4&query=черкассы

suggest_location(#'SuggestLocation'{ref=Ref, input=Input}, _UserId) ->
  Locs = case im_common:length_utf8(Input) >= 3 of
           false -> [];
           true  -> get_locations_suggestions(Input)
         end,
  #'SuggestLocationResp'{ref=Ref, locations=Locs}.

get_locations_suggestions(Input) ->
  Url = locations_suggestions_url(Input),
  Data = get_json(Url),

  case proplists:get_value(<<"status">>, Data) of
    <<"OK">> ->
      lists:map(fun({Loc}) ->
                  Description = proplists:get_value(<<"description">>, Loc, <<>>),
                  PlaceId = proplists:get_value(<<"place_id">>, Loc, <<>>),
                  #'LocationEntity'{
                    id=PlaceId,
                    name=Description
                  }
                end, proplists:get_value(<<"predictions">>, Data, []));
    _        -> []
  end.

location_by_gps(#'LocationByGps'{ref=Ref, coordinates=[Lat,Lng], locale=Locale}, _UserId) ->
  Url = geocode_latlng_url({Lat,Lng}, Locale),
  Data = get_json(Url),
  LocationEntity = case proplists:get_value(<<"status">>, Data) of
                     <<"OK">> ->
                       [{Result}|_] = proplists:get_value(<<"results">>, Data),
                       Address = proplists:get_value(<<"formatted_address">>, Result, <<>>),
                       PlaceId = proplists:get_value(<<"place_id">>, Result, <<>>),
                       #'LocationEntity'{id=PlaceId, name=Address};
                     _        -> #'LocationEntity'{}
                   end,


  #'LocationByGpsResp'{ref=Ref, location=LocationEntity}.


location_by_id(LocId) ->
  Url = place_id_url(LocId),
  Data = get_json(Url),
  Data.

submit_location(LocId) ->
  Data = location_by_id(LocId),
  case proplists:get_value(<<"status">>, Data) of
    <<"OK">> ->
      {Result} = proplists:get_value(<<"result">>, Data),
      {Geometry} = proplists:get_value(<<"geometry">>, Result, []),
      {Loc} = proplists:get_value(<<"location">>, Geometry, []),
      Lat = proplists:get_value(<<"lat">>, Loc, 0),
      Lng = proplists:get_value(<<"lng">>, Loc, 0),

      FormattedAddress = proplists:get_value(<<"formatted_address">>, Result, <<>>),

      Location = #channel_location{
        id=LocId,
        name=FormattedAddress,
        thumb= <<>>,
        coordinates=[Lat,Lng]
      },
      ctail:put(Location),
      LocId;
    _        -> undefined
   end.

%%location_by_gps(#'LocationByGps'{ref=Ref}, _) ->
%%  #'LocationByGpsResp'{ref = Ref,location = #'LocationEntity'{}}.


%%submit_location(#'SubmitLocation'{ref=Ref, location=#'LocationEntity'{id=PlaceId}}, _UserId) ->
%%submit_location(PlaceId, _UserId) ->
%%  Url = place_id_url(PlaceId),
%%  Data = get_json(Url),
%%
%%  case proplists:get_value(<<"status">>, Data) of
%%    <<"OK">> ->
%%      {Result} = proplists:get_value(<<"result">>, Data),
%%      {Geometry} = proplists:get_value(<<"geometry">>, Result, []),
%%      {Loc} = proplists:get_value(<<"location">>, Geometry, []),
%%      Lat = proplists:get_value(<<"lat">>, Loc, 0),
%%      Lng = proplists:get_value(<<"lng">>, Loc, 0),
%%
%%      FormattedAddress = proplists:get_value(<<"formatted_address">>, Result, <<>>),
%%      Location = #channel_location{
%%        id=ctail:next_id(), %% fetch ID e.g. :che :kiev
%%        name=FormattedAddress,      %% save all langs
%%        thumb= <<>>,
%%        location_tag=LocationTag,
%%        coordinates=[Lat,Lng],
%%        radius=calc_radius(FormattedAddress)
%%      },
%%      ok = ctail:put(Location),
%%      im_dto:format_location(Location);
%%    _        -> im_dto:format_location(#channel_location{})
%%  end.

%%  #'SubmitLocationResp'{ref=Ref, location=LocationEntity}.

calc_radius(FormattedAddress) ->
  Url = geocode_address_url(FormattedAddress),
  Data = get_json(Url),

  case proplists:get_value(<<"status">>, Data) of
    <<"OK">> ->
      [{Result}|_] = proplists:get_value(<<"results">>, Data),
      {Geometry} = proplists:get_value(<<"geometry">>, Result, {[]}),
      Bounds = proplists:get_value(<<"bounds">>, Geometry, undefined),
      {{NELat,NELng},{SWLat,SWLng}} = case Bounds of
                                        undefined ->
                                          Viewport = proplists:get_value(<<"viewport">>, Geometry, undefined),
                                          get_coordinate_pairs(Viewport);
                                        _         -> get_coordinate_pairs(Bounds)
                                      end,
      calc_half_distance({{NELat,NELng},{SWLat,SWLng}});

    _        -> 1000
  end.

get_json(Url) ->
  case im_http:get(Url, [{"User-Agent", "ConnectPlatformBot/1.x (metadata fetcher)"}]) of
    {ok, 200, Body, _} ->
      try jiffy:decode(Body) of
        {D}  -> D;
        null -> []
      catch
        _:_  -> []
      end;
    _ -> []
  end.

calc_half_distance({{Lat1, Lng1}, {Lat2, Lng2}}) ->
  Pi = math:pi(),

  Lat1Rad = Lat1 * Pi / 180,
  Lng1Rad = Lng1 * Pi / 180,
  Lat2Rad = Lat2 * Pi / 180,
  Lng2Rad = Lng2 * Pi / 180,

  Cl1 = math:cos(Lat1Rad),
  Cl2 = math:cos(Lat2Rad),
  Sl1 = math:sin(Lat1Rad),
  Sl2 = math:sin(Lat2Rad),

  Delta = Lng2Rad - Lng1Rad,
  CDelta = math:cos(Delta),
  SDelta = math:sin(Delta),

  Y = math:sqrt(
    math:pow(Cl2 * SDelta, 2) +
    math:pow(Cl1*Sl2 - Sl1*Cl2 * CDelta, 2)
  ),
  X = Sl1 * Sl2 + Cl1*Cl2*CDelta,

  Ad = math:atan2(Y, X),
  Dist = Ad * ?EARTH_RADIUS,
  HalfDist = Dist / 2,

  round(HalfDist).

get_coordinate_pairs(undefined) -> {{0,0},{0,0}};
get_coordinate_pairs(
  {[
    {<<"northeast">>, {[{<<"lat">>,NELat},{<<"lng">>,NELng}]}},
    {<<"southwest">>, {[{<<"lat">>,SWLat},{<<"lng">>,SWLng}]}}]
  }) ->
  {{NELat,NELng},{SWLat,SWLng}}.

place_id_url(Input) -> place_id_url(Input, ?FALLBACK_LOCALE).
place_id_url(Input, Locale) ->
  Url = <<"https://maps.googleapis.com">>,
  Path = <<"/maps/api/place/details/json">>,
  Query = [
    {<<"key">>, ?API_KEY},
    {<<"placeid">>, Input},
    {<<"language">>, Locale}
  ],

  hackney_url:make_url(Url, Path, Query).

locations_suggestions_url(Input) ->
  Url = <<"https://maps.googleapis.com">>,
  Path = <<"/maps/api/place/autocomplete/json">>,
  Query = [
    {<<"types">>, <<"geocode">>},
    {<<"key">>, ?API_KEY},
    {<<"input">>, im_common:format_utf8(Input)}
  ],

  hackney_url:make_url(Url, Path, Query).

geocode_address_url(Input) ->
  Url = <<"https://maps.googleapis.com">>,
  Path = <<"/maps/api/geocode/json">>,
  Query = [
    {<<"key">>, ?API_KEY},
    {<<"address">>, im_common:format_utf8(Input)}
  ],

  hackney_url:make_url(Url, Path, Query).

geocode_latlng_url({Lat,Lng}, Locale) when is_number(Lat) andalso is_number(Lng) ->
  LatBin = float_to_binary(Lat/1,[{decimals, 7}]),
  LngBin = float_to_binary(Lng/1,[{decimals, 7}]),

  Url = <<"https://maps.googleapis.com">>,
  Path = <<"maps/api/geocode/json">>,
  Query = [
    {<<"key">>, ?API_KEY},
    {<<"result_type">>, <<"locality">>},
    {<<"language">>, validate_locale(Locale)},
    {<<"latlng">>, <<LatBin/binary, <<",">>/binary, LngBin/binary>>}
  ],

  hackney_url:make_url(Url, Path, Query);
geocode_latlng_url({_,_}, Locale) -> geocode_latlng_url({0,0}, Locale).

validate_locale(Locale) ->
  case lists:member(Locale, get_langs()) of
    true  -> Locale;
    false -> ?FALLBACK_LOCALE
  end.

get_langs() -> [
  <<"ar">>,
  <<"bg">>,
  <<"bn">>,
  <<"ca">>,
  <<"cs">>,
  <<"da">>,
  <<"de">>,
  <<"el">>,
  <<"en">>,
  <<"en-AU">>,
  <<"en-GB">>,
  <<"es">>,
  <<"eu">>,
  <<"fa">>,
  <<"fi">>,
  <<"fil">>,
  <<"fr">>,
  <<"gl">>,
  <<"gu">>,
  <<"hi">>,
  <<"hr">>,
  <<"hu">>,
  <<"id">>,
  <<"it">>,
  <<"iw">>,
  <<"ja">>,
  <<"kn">>,
  <<"ko">>,
  <<"lt">>,
  <<"lv">>,
  <<"ml">>,
  <<"mr">>,
  <<"nl">>,
  <<"no">>,
  <<"pl">>,
  <<"pt">>,
  <<"pt-BR">>,
  <<"pt-PT">>,
  <<"ro">>,
  <<"ru">>,
  <<"sk">>,
  <<"sl">>,
  <<"sr">>,
  <<"sv">>,
  <<"ta">>,
  <<"te">>,
  <<"th">>,
  <<"tl">>,
  <<"tr">>,
  <<"uk">>,
  <<"vi">>,
  <<"zh-CN">>,
  <<"zh-TW">>
].
