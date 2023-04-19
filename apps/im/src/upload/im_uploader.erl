-module(im_uploader).

-include("im_common.hrl").

-export([upload/2]).

upload(#'UploadMedia'{ref=Ref, media=Media}, UserId) ->
  Result = case Media#'InputMediaEntity'.totalChunks =:= undefined orelse Media#'InputMediaEntity'.totalChunks =:= 0 of
    true ->
      handle_upload(#'InputMediaEntity'{
        type= Media#'InputMediaEntity'.type,
        name = Media#'InputMediaEntity'.name,
        media = Media#'InputMediaEntity'.media,
        mime = Media#'InputMediaEntity'.mime,
        width = Media#'InputMediaEntity'.width,
        height = Media#'InputMediaEntity'.height,
        thumbnail = Media#'InputMediaEntity'.thumbnail,
        duration = Media#'InputMediaEntity'.duration
      }, UserId);
    false ->
      handle_upload(Media, UserId)
  end,
  case Result of
    {ok, Url} -> #'UploadMediaResp'{ref=Ref, url=Url, media=Media};
    {error, empty} -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_INVALID_MESSAGE};
    {error, _} -> #'ErrorResp'{ref=Ref, code=?ERROR_CODE_UNKNOWN}
  end.

handle_upload(Media, UserId) ->
  case application:get_env(im, filestore) of
    {ok, local} ->
      im_filestore_local:upload(Media, UserId);
    {ok, s3} ->
      case im_filestore_s3:upload(Media) of
        {ok, Url} -> {ok, Url};
        {aws_error, Error} -> {error, Error}
      end;
    _ -> {error, unknown_filestore}
  end.
