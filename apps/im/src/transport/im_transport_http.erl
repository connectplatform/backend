-module(im_transport_http).

-include("im_common.hrl").

-export([handle/2]).

handle(cors, _) -> ok;
handle(Msg, Req) ->
  {Token, _} = cowboy_req:qs_val(<<"token">>, Req),
  {Locale, _} = cowboy_req:qs_val(<<"locale">>, Req),
  Version = im_transport_router:parse_version_number(Req),

  Result = case Token =:= undefined of
    true ->
      im_transport_router:process(Msg, undefined, undefined, Version);
    false ->
      case im_init:authenticate(binary_to_list(Token), Locale) of
        {ok, User, TokenRec} ->
          UserId1 = im_common:parse_id(User#im_usr.id),
          im_user_state:checkin(http, TokenRec),
          im_roster_chat:worker(UserId1),
          im_user_settings:init(UserId1),
          ProcessedResult = im_transport_router:process(Msg, User, TokenRec, Version),
          im_user_state:checkout(UserId1, self),
          im_roster_chat:execute(UserId1, fun(User1) ->
            {ok, User1#im_usr{lastSeen=sm:now()}}
          end),
          ProcessedResult;
        {error, token_not_found} ->
          im_transport_router:process(Msg, undefined, undefined, Version)
      end
  end,

  case Result of
    none                          -> {ok, #sm_response{}};
    {ok, ProcessedData, Response} -> {ok, im_transport_router:format_record(ProcessedData), Response};
    {ok, ProcessedData}           -> {ok, im_transport_router:format_record(ProcessedData), #sm_response{}};
    ProcessedData                 -> {ok, im_transport_router:format_record(ProcessedData), #sm_response{}}
  end.
