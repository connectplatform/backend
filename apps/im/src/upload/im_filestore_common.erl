-module(im_filestore_common).

-include("im_common.hrl").

-export([prepare_filename/1, get_file_type_folder/1, generate/0]).

prepare_filename(FilenameOrigin) ->
    Ext = filename:extension(im_common:ensure_list(FilenameOrigin)),
    Generated = generate(),
    Filename = Generated ++ Ext,
    Filename.

generate()                          -> generate(35).
generate(Len)                       -> generate(Len, undefined).
generate(Len, Preffix)              -> generate(Len, Preffix, undefined).
generate(Len, undefined, undefined) -> generate(Len, "", "");
generate(Len, undefined, Suffix)    -> generate(Len, "", Suffix);
generate(Len, Preffix, undefined)   -> generate(Len, Preffix, "");
generate(Len, Preffix, Suffix)      ->
  Preffix ++ im_common:random_string(Len, chars_and_numbers) ++ integer_to_list(sm:now()) ++ Suffix.

get_file_type_folder(Type) ->
  case Type of
    ?MEDIA_TYPE_IMAGE    -> {ok, "image"};
    ?MEDIA_TYPE_VIDEO    -> {ok, "video"};
    ?MEDIA_TYPE_AUDIO    -> {ok, "audio"};
    ?MEDIA_TYPE_DOCUMENT -> {ok, "document"};
    _                    -> {ok, "file"}
  end.
