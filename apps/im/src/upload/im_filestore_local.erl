-module(im_filestore_local).

-include("im_common.hrl").

%% API
-export([upload/2]).


upload(Msg=#'InputMediaEntity'{url=Url, name=Name, type=Type, media=Media}, UserId) ->
  {ok, Path}  = application:get_env(im, filestore_local_path),

  LookupFileResult = case Url of
    Url1 when is_binary(Url1) ->
      [Hash|_] = lists:reverse(string:split(Url, "/", all)),
      ctail:get(im_lookup_file, Hash);
    _ ->
      {error, invalid_url}
  end,

  case byte_size(Media) > 0 of
    true ->
      case LookupFileResult of
        {ok, LookupFileRecord} ->
          case ctail:get(im_file, im_common:parse_id(LookupFileRecord#im_lookup_file.fileId)) of
            {ok, FileRecord} ->
              FullPath = Path ++ "/" ++ binary_to_list(FileRecord#im_file.path) ++ binary_to_list(FileRecord#im_file.name),
              case file:write_file(FullPath, Media, [append]) of
                ok    -> {ok, Url};
                Error -> {error, Error}
              end;
            _ ->
              {error, not_found}
          end;
        {error, _} ->
          Name1 = im_filestore_common:prepare_filename(Name),
          case im_filestore_common:get_file_type_folder(Type) of
            {ok, SubFolder} ->
              Path1 = SubFolder ++ "/" ++ string:sub_string(Name1, 1, 2) ++ "/",
              FullPath = Path ++ "/" ++ Path1 ++ Name1,
              filelib:ensure_dir(Path ++ "/" ++ Path1),

              im_logger:error(UserId, "[FilestoreLocal] Writing file ~p", [FullPath]),

              case file:write_file(FullPath, Media) of
                ok ->
                  case store_to_db(Msg, Path1, Name1) of
                    {ok, U}   -> {ok, U};
                    {error, Error} -> {error, Error}
                  end;
                Error -> {error, Error}
              end;
            {error, Error} ->
              im_logger:error(UserId, "[FilestoreLocal] Unable to get folder for this type of file", []),
              {error, Error}
          end
      end;
    false ->
      {error, empty}
  end.

store_to_db(Msg, Path, Name) ->
  Id = ctail:next_id(),
  M = parse_input_media(Msg),
  ctail:put(M#im_file{id=Id, path=Path, name=Name}),
  Hash = im_filestore_common:generate(),
  ctail:put(#im_lookup_file{hash=list_to_binary(Hash), fileId=Id}),
  {ok, im_common:base_url() ++ "/media/" ++ Hash}.

parse_input_media(Msg=#'InputMediaEntity'{}) ->
  #im_file{
    mime      = im_common:ensure_list(Msg#'InputMediaEntity'.mime),
    width     = Msg#'InputMediaEntity'.width,
    height    = Msg#'InputMediaEntity'.height,
    thumbnail = Msg#'InputMediaEntity'.thumbnail,
    duration  = Msg#'InputMediaEntity'.duration,
    size      = erlang:byte_size(Msg#'InputMediaEntity'.media)
  }.
