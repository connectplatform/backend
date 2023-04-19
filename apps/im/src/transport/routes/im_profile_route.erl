-module(im_profile_route).

-include("im_common.hrl").

-export([handle/1]).

handle(Req) ->
  {UserId, _} = cowboy_req:binding(id, Req),
  User = try im_roster_chat:get(im_common:parse_id(UserId)) catch _:_ -> undefined end,
  case User =:= undefined of
    true ->
      im_logger:error(undefined, "[Static] Can't find user, unknown id: ~p", [UserId]),
      {ok, #sm_response{
        status=404
      }};
    false ->
      Variables = [
        {id, im_common:format_id(User#im_usr.id)},
        {name, User#im_usr.name},
        {bio, User#im_usr.bio},
        {thumbnail, User#im_usr.thumbnail}
      ],
      FilePath = filename:join(
        filename:dirname(filename:dirname(code:which(?MODULE))),
        "priv/templates/profile.html"),
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
