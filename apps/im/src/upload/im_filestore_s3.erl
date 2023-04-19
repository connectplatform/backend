-module(im_filestore_s3).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include("im_common.hrl").

%% API
-export([upload/1]).

upload(#'InputMediaEntity'{name=Name, type=_Type, media=Media}) ->
    Filename         = im_filestore_common:prepare_filename(Name),
    {ok, UrlPrefix}  = application:get_env(im, s3_url_prefix),
    {ok, RemoteDir}  = application:get_env(im, s3_remote_dir),
    {ok, BucketName} = application:get_env(im, s3_bucket_name),
    {ok, AccessKey}  = application:get_env(im, s3_access_key),
    {ok, SecretKey}  = application:get_env(im, s3_secret_key),
    {ok, Region}     = application:get_env(im, s3_region),
    FullRemotePath   = RemoteDir ++ "/" ++ Filename,

%%    Folder = im_filestore_common:get_file_type_folder(_Type),
%%    io:format("UrlPrefix: ~p", [UrlPrefix]),
%%    io:format("FullRemotePath: ~p", [FullRemotePath]),
%%    io:format("RemoteDir: ~p", [RemoteDir]),
%%    io:format("BucketName: ~p", [BucketName]),
%%    io:format("AccessKey: ~p", [AccessKey]),
%%    io:format("SecretKey: ~p", [SecretKey]),
%%    io:format("Region: ~p", [Region]),
%%    io:format("UrlPrefix: ~p", [UrlPrefix]),
%%    io:format("Folder: ~p", [Folder]),
%%    io:format("FullUrl: ~p~n~n~n", [UrlPrefix ++ FullRemotePath]),

    case erlcloud_s3:put_object(BucketName, FullRemotePath, Media, #aws_config{
        access_key_id = AccessKey,
        secret_access_key = SecretKey,
        aws_region = Region
    }) of
        {aws_error, Error} -> {aws_error, Error};
        _                  -> {ok, UrlPrefix ++ FullRemotePath}
    end.

