-module(im_media_route).

-include("im_common.hrl").

-export([handle/1]).

handle(Req) ->
  {Hash, _} = cowboy_req:binding(hash, Req),
  {ok, Path}  = application:get_env(im, filestore_local_path),
  case ctail:get(im_lookup_file, Hash) of
    {ok, LookupFileRecord} ->
      case ctail:get(im_file, im_common:parse_id(LookupFileRecord#im_lookup_file.fileId)) of
        {ok, FileRecord} ->
          FullPath = Path ++ "/" ++ binary_to_list(FileRecord#im_file.path) ++ binary_to_list(FileRecord#im_file.name),
          case file:read_file(FullPath) of
            {ok, FileContent} ->
              {ok, #sm_response{
                status  = 200,
                headers = [{<<"content-type">>, FileRecord#im_file.mime}],
                body    = FileContent
              }};
            _ ->
              im_logger:error(undefined, "[Media][~p] file not found ~p", [Hash, FullPath]),
              {ok, #sm_response{
                status  = 500,
                headers = [{<<"content-type">>, <<"text/plain">>}],
                body    = <<"Cannot read file">>
              }}
          end;
        _ ->
          im_logger:error(undefined, "[Media][~p] im_file not found ~p", [Hash, LookupFileRecord]),
          {ok, #sm_response{
            status  = 404,
            headers = [{<<"content-type">>, <<"text/plain">>}],
            body    = <<"File not found">>
          }}
      end;
    _ ->
      im_logger:error(undefined, "[Media][~p] im_lookup_file not found", [Hash]),
      {ok, #sm_response{
        status  = 403,
        headers = [{<<"content-type">>, <<"text/plain">>}],
        body    = <<"Hash expired or not exist">>
      }}
  end.
