-module(im_og).

-include("im_common.hrl").
-include_lib("hackney/include/hackney_lib.hrl").

-define(OG_DATA_TTL, 60 * 60 * 24 * 7 * 1000). %% 7 days

-export([locate_by_ip/2]).
-export([fetch/2, fetch_url/4]).

locate_by_ip(#'LocateByIp7'{ref=Ref, ip=Ip}, UserId) ->
  FinalUrl = "http://" ++ im_common:ensure_list(sm:env(im, og_url)) ++ "/fetch/ip-location?ip=" ++ im_common:ensure_list(Ip),
  io:format("[OpenGraph] Fetch url: ~p", [FinalUrl]),
  case im_http:get(FinalUrl, [], [{recv_timeout, 65000}]) of
    {ok, 200, Body, _} ->
      Response = jsx:decode(Body),

      Country = proplists:get_value(<<"country_name">>, Response),
      Region = proplists:get_value(<<"region_name">>, Response),
      City = proplists:get_value(<<"city_name">>, Response),
      Lat = im_common:binary_to_num(proplists:get_value(<<"latitude">>, Response)),
      Lng = im_common:binary_to_num(proplists:get_value(<<"longitude">>, Response)),

      #'LocateByIpResp7'{ref=Ref, location=[Lat, Lng], country=Country, region=Region, city=City};
    {_, _, BodyOrReason, _} ->
      im_logger:error(UserId, "[OG] Failed to make locate request: ~p", [BodyOrReason]),
      #'ErrorResp'{message="Failed to call external API"}
  end.

fetch(#'FetchOgData'{ref=Ref, url=Url, forceRefetch=ForceRefetch}, UserId) ->
  Url1 = im_common:ensure_binary(im_common:ensure_list(Url)),
  ForceRefetch1 = ForceRefetch =:= true,

  case fetch_url(Url1, false, ForceRefetch1, UserId) of
    {ok, OgData} ->
      #'FetchOgDataResp'{ref=Ref, ogData=im_dto:format_ogdata(OgData)};
    {error, Reason} ->
      im_logger:error(UserId, "Failed to fetch og data. Url: ~p, Reason: ~p", [Url, Reason]),
      #'ErrorResp'{ref=Ref, code=?ERROR_CODE_UNKNOWN}
  end.

fetch_url(Url, CacheOnly, ForceRefetch, UserId) ->
  case ForceRefetch =:= true of
    true -> refetch(Url, UserId, undefined);
    false ->
      case ctail:get(im_ogdata, Url) of
        {ok, OgData} ->
          % case sm:now() - OgData#im_ogdata.fetched < ?OG_DATA_TTL of
          %   true -> {ok, OgData};
          %   false ->
          %     case CacheOnly =:= true of
          %       true -> {ok, OgData};
          %       false -> refetch(Url, UserId, OgData)
          %     end
          % end;
          {ok, OgData};
        {error, _} ->
          case CacheOnly =:= true of
            true -> {error, cache_miss};
            false -> refetch(Url, UserId)
          end
      end
  end.

refetch(Url, UserId) -> refetch(Url, UserId, undefined).
refetch(Url, UserId, OutdatedOgData) ->
  Result = case im:is_debug() of
    true ->
      UrlAsList = im_common:ensure_list(Url),
      Refetch = string:str(UrlAsList, "?refetch=1") > 0,
      Title = case Refetch of
        true -> "Example title refetched";
        false -> "Example title"
      end,
      JsonStruct = {struct, [
        {<<"title">>, Title},
        {<<"description">>, <<"Example description">>},
        {<<"image">>, <<"http://example.com/image.jpg">>},
        {<<"domain">>, <<"example.com">>},
        {<<"json">>, im_common:format_utf8("{\"og:title\": \"" ++ Title ++ "\"}")},
        {<<"body">>, <<"<html></html>">>},
        {<<"context">>, <<"Example context">>},
        {<<"media">>, {array, [
          {struct, [
            {<<"image">>, <<"http://example/media/0/image.jpg">>},
            {<<"thumbnail">>, <<"http://example/media/0/thumbnail.jpg">>},
            {<<"description">>, <<"Media description">>}
          ]}
        ]}}
      ]},
      Json = im_common:format_utf8(yaws_json2:encode(JsonStruct)),
      Url1 = im_common:format_utf8(re:replace(UrlAsList, "\\?refetch=1", "", [{return,list}])),
      {Url1, {ok, 200, im_common:ensure_list(Json), []}};
    false ->
      UrlEncoded = im_encode_uri:encode(im_common:ensure_list(Url)),
      Token = im_common:ensure_list(im_user_state:attr(UserId, token)),
      Token1 = case Token =:= undefined of
        true -> "";
        false -> Token
      end,
      FinalUrl = "http://" ++ im_common:ensure_list(sm:env(im, og_url)) ++ "/fetch?token=" ++ Token1 ++ "&url=" ++ UrlEncoded,
      % im_logger:debug(undefined, "[OpenGraph] Fetch url: ~p", [FinalUrl]),
      {Url, im_http:get(FinalUrl, [], [{recv_timeout, 65000}])}
  end,

  case Result of
    {Url2, {ok, 200, Body, _}} ->
      case yaws_json2:decode_string(im_common:ensure_list(Body)) of
        {ok, {struct, Props}} ->
          OgData1 = #im_ogdata{
            id=Url2,
            favicon=im_common:ensure_binary(get_favicon_path(Url)),
            domain=im_common:ensure_binary(proplists:get_value("domain", Props)),
            title=im_common:ensure_binary(proplists:get_value("title", Props)),
            description=im_common:ensure_binary(proplists:get_value("description", Props)),
            image=im_common:ensure_binary(proplists:get_value("image", Props)),
            json=im_common:ensure_binary(yaws_json2:encode(proplists:get_value("json", Props))),
            context=im_common:ensure_binary(proplists:get_value("context", Props)),
            body=im_common:ensure_binary(proplists:get_value("body", Props)),
            media=format_media_list(proplists:get_value("media", Props, [])),
            fetched=sm:now()
          },
          % im_logger:debug(undefined, "[OpenGraph] Fetched: ~p", [OgData1]),
          ok = ctail:put(OgData1),
          {ok, OgData1};
        _ ->
          im_logger:error(undefined, "[OpenGraph] Fetch failed: malformed json", []),
          {error, malformed_json}
      end;
    {_, Error} ->
      im_logger:error(undefined, "[OpenGraph] Fetch failed: ~p", [Error]),
      case OutdatedOgData =:= undefined of
        true -> Error;
        false -> {ok, OutdatedOgData}
      end
  end.

format_media_list({array, Media}) -> format_media_list(Media);
format_media_list(Media) ->
  [format_media(M) || M <- Media].

format_media({struct, Props}) ->
  {<<"ogmedia">>, [
    {<<"image">>, proplists:get_value("image", Props)},
    {<<"thumbnail">>, proplists:get_value("thumbnail", Props)},
    {<<"description">>, proplists:get_value("description", Props)}
  ]}.

get_favicon_path(Url) ->
  case im:is_debug() of
    true ->
      <<"http://example.com/favicon.ico">>;
    false ->
      #hackney_url{scheme=Scheme, netloc=NetLoc} = hackney_url:parse_url(Url),
      IcoUrl = #hackney_url{scheme=Scheme, netloc=NetLoc, path = <<"/favicon.ico">>},
      IcoPath = hackney_url:unparse_url(IcoUrl),

      case im_http:head(IcoPath) of
        {ok, 200, _, _} -> IcoPath;
        _ ->
          PngUrl = #hackney_url{scheme=Scheme, netloc=NetLoc, path = <<"/favicon.png">>},
          PngPath = hackney_url:unparse_url(PngUrl),

          case im_http:head(PngPath) of
            {ok, 200, _, _} -> PngPath;
            _ -> undefined
          end
      end
  end.
