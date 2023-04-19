-module(im_feed_post_route).

-include("im_common.hrl").

-export([handle/1]).

handle(Req) ->
  {FeedPostId, _} = cowboy_req:binding(id, Req),
  case ctail:get(im_feed_post, im_common:parse_id(FeedPostId)) of
    {error, _} ->
      im_logger:error(undefined, "[Static] Can't find feed post, unknown id: ~p", [FeedPostId]),
      {ok, #sm_response{
        status=404
      }};
    {ok, Post} ->
      Thumbnail = case Post#im_feed_post.media =:= [] of
        true -> "";
        false ->
          {<<"media">>, Props} = hd(Post#im_feed_post.media),
          proplists:get_value(<<"thumbnail">>, Props)
      end,
      io:format("Thumbnail: ~p", [Thumbnail]),
      Variables = [
        {title, Post#im_feed_post.title},
        {descr, Post#im_feed_post.descr},
        {thumbnail, Thumbnail}
      ],
      FilePath = filename:join(
        filename:dirname(filename:dirname(code:which(?MODULE))),
        "priv/templates/feed-post.html"),
      erlydtl:compile(FilePath, profile_template),
      case profile_template:render(Variables) of
        {ok, Output} ->
          {ok, #sm_response{
            status=200,
            headers=[{<<"content-type">>, <<"text/html">>}],
            body=Output
          }};
        Error ->
          im_logger:error(undefined, "[Static] Template render error: ~p", [Error])
      end
  end.
