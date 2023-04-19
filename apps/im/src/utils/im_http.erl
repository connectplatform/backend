-module(im_http).

-include("im_common.hrl").

-export([request/2, request/3, request/4, request/5]).
-export([get/1, get/2, get/3]).
-export([post/2, post/3, post/4]).
-export([put/2, put/3, put/4]).
-export([delete/1, delete/2, delete/3]).
-export([head/1, head/2, head/3]).

get(Url) -> get(Url, [], []).
get(Url, Headers) -> get(Url, Headers, []).
get(Url, Headers, Options) ->
  request(get, Url, <<>>, Headers, Options).

post(Url, Body) -> post(Url, Body, []).
post(Url, Body, Headers) -> post(Url, Body, Headers, []).
post(Url, Body, Headers, Options) ->
  request(post, Url, Body, Headers, Options).

put(Url, Body) -> put(Url, Body, []).
put(Url, Body, Headers) -> put(Url, Body, Headers, []).
put(Url, Body, Headers, Options) ->
  request(put, Url, Body, Headers, Options).

delete(Url) -> delete(Url, []).
delete(Url, Headers) -> delete(Url, Headers, []).
delete(Url, Headers, Options) ->
  request(delete, Url, <<>>, Headers, Options).

head(Url) -> head(Url, []).
head(Url, Headers) -> head(Url, Headers, []).
head(Url, Headers, Options) ->
  request(head, Url, <<>>, Headers, Options).

request(Method, Url) -> request(Method, Url, <<>>).
request(Method, Url, Body) -> request(Method, Url, Body, []).
request(Method, Url, Body, Headers) -> request(Method, Url, Body, Headers, []).
request(Method, Url, Body, Headers, Options) -> request(Method, Url, Body, Headers, Options, 0).
request(Method, Url, Body, Headers, Options, Retries) ->
  Response = try
    hackney:request(Method, Url, Headers, Body, [{follow_redirect, false}] ++ Options)
  catch
    Type:Exception ->
      im_logger:error(undefined, "Hackney HTTP Request Error. Type: ~p, Exception: ~p", [Type, Exception]),
      {error, undefined, Exception, []}
  end,

  case Response of
    {ok, RespStatus, RespHeaders} ->
      {ok, RespStatus, undefined, RespHeaders};
    {ok, RespStatus, RespHeaders, Ref} ->
      RespBody = case hackney:body(Ref) of
        {ok, Body1} ->
          case Body1 =:= <<>> of
            true -> undefined;
            false -> Body1
          end;
        _ ->
          undefined
      end,
      {ok, RespStatus, RespBody, RespHeaders};
    {error, Reason} ->
      case Retries =:= 3 of
        true -> {error, undefined, Reason, []};
        false ->
          im_logger:info(undefined, "[Http] Request failed. Url: ~p. One more try...", [Url]),
          request(Method, Url, Body, Headers, Options, Retries + 1)
      end
  end.
