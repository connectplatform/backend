-module(im_report).

-include("im_common.hrl").

-define(TOPIC_SIZE, 250).
-define(BODY_SIZE, 1000).

-export([add/2, add_with_locale/2, send_report_email/4]).

add(#'AddReport'{name=Name, email=Email, client=Client, topic=Topic, information=Information, message=Message}, User=#im_usr{}) ->
  AddReportWithLocale = #'AddReportWithLocale'{
                           name = Name,
                           email = Email,
                           client = Client,
                           topic = Topic,
                           information = Information,
                           message = Message,
                           locale = ?DEFAULT_LOCALE
                          },
  add_with_locale(AddReportWithLocale, User).

add_with_locale(
  #'AddReportWithLocale'{
     name=Name,
     email=Email,
     client=Client,
     topic=Topic,
     information=Information,
     message=Message,
     locale=Locale
    },
  #im_usr{
     id=UserId,
     phone=Phone
    }
 ) ->
  case
    (Name =:= undefined) orelse (Name =:= <<>>) orelse
    (Email =:= undefined) orelse (Email =:= <<>>) orelse
    (Topic =:= undefined) orelse (Topic =:= <<>>) orelse
    (Information =:= undefined) orelse (Information =:= <<>>) orelse
    (length(unicode:characters_to_list(Information, utf8)) > ?BODY_SIZE) orelse
    (length(unicode:characters_to_list(Topic, utf8)) > ?TOPIC_SIZE)
  of
    true  -> {ok, #'ErrorResp'{ code=?ERROR_CODE_REPORT_FIELDS_INVALID,
                                ref=0, messageParams=[],
                                messageType= <<"report.field.length.not.valid">>,
                                message=im_common:format_utf8(im_trans:t(<<"report.field.length.not.valid">>))}};
    false ->
      FeedId = Message#'Message'.feedId,
      FeedType = Message#'Message'.feedType,

      ctail:put(#im_report{
                   id            = ctail:next_id(),
                   userId        = UserId,
                   toUserId      = Message#'Message'.message#'MessageEntity'.origin,
                   name          = Name,
                   email         = Email,
                   phone         = Phone,
                   client        = Client,
                   topic         = Topic,
                   information   = Information,
                   feedId        = im_common:parse_id(FeedId),
                   feedType      = FeedType,
                   messageEntity = format_message_entity(Message#'Message'.message),
                   createdAt     = sm:now(),
                   status        = <<"new">>
                  }),

      send_report_email(
        im_common:ensure_binary(Email),
        im_common:ensure_binary(Topic),
        Message#'Message'.message#'MessageEntity'.kind,
        im_common:ensure_binary(Locale)
       ),

      {ok, #'AddReportResp'{}}
  end.

format_message_entity(MessageEntity) ->
  MediaRaw = case MessageEntity#'MessageEntity'.media of
               List when is_list(List) -> List;
               _ -> []
             end,
  MediaVal = lists:foldl(fun(OneMedia, Acc) ->
                             case is_record(OneMedia, 'MediaEntity') of
                               true ->
                                 M = [
                                      {type, OneMedia#'MediaEntity'.type},
                                      {link, OneMedia#'MediaEntity'.link},
                                      {thumbnail, OneMedia#'MediaEntity'.thumbnail},
                                      {width, OneMedia#'MediaEntity'.width},
                                      {height, OneMedia#'MediaEntity'.height},
                                      {duration, OneMedia#'MediaEntity'.duration},
                                      {name, OneMedia#'MediaEntity'.name},
                                      {size, OneMedia#'MediaEntity'.size}
                                     ],
                                 [M|Acc];
                               false -> Acc
                             end
                         end, [], MediaRaw),

  [
   {id,                  MessageEntity#'MessageEntity'.id},
   {type,                MessageEntity#'MessageEntity'.type},
   {systemMessageType,   undefined},
   {systemMessageParams, undefined},
   {kind,                MessageEntity#'MessageEntity'.kind},
   {created,             MessageEntity#'MessageEntity'.created},
   {origin,              MessageEntity#'MessageEntity'.origin},
   {recipient,           MessageEntity#'MessageEntity'.recipient},
   {payload,             MessageEntity#'MessageEntity'.payload},
   {media,               MediaVal},
   {starred,             MessageEntity#'MessageEntity'.starred},
   {geo,                 MessageEntity#'MessageEntity'.geo}
  ].

send_report_email(EmailAddress, MessageTopic, MessageKind, Locale) ->

  Relay = sm:env(im, smtp_relay),
  Username = sm:env(im, smtp_username),
  Password = sm:env(im, smtp_password),

  Locale1 = case lists:member(Locale, ?LOCALES) of
              true  -> Locale;
              false -> ?DEFAULT_LOCALE
            end,

  FilePath = filename:join(
               filename:dirname(filename:dirname(code:which(?MODULE))),
               "priv/emails/report/" ++ binary_to_list(Locale1) ++ ".html"
              ),

  KindKey = case MessageKind of
              ?MESSAGE_KIND_AUDIO    -> <<"message_audio">>;
              ?MESSAGE_KIND_DOCUMENT -> <<"message_doc">>;
              ?MESSAGE_KIND_IMAGE    -> <<"message_image">>;
              ?MESSAGE_KIND_MAP      -> <<"message_map">>;
              ?MESSAGE_KIND_STICKER  -> <<"message_sticker">>;
              ?MESSAGE_KIND_VIDEO    -> <<"message_video">>;
              _                      -> <<"message_text">>
            end,

  ReportType = im_common:ensure_binary(im_trans:t(Locale1, KindKey)),

  {ok, Html} = file:read_file(FilePath),
  Html1 = binary:replace(Html, <<"%complaint_type%">>, MessageTopic, [global]),
  Html2 = binary:replace(Html1, <<"%message_type%">>, ReportType),

  Email = {
    Username,
    [EmailAddress],
    mimemail:encode({
                      <<"text">>,
                      <<"html">>,
                      [
                       {<<"From">>, Username},
                       {<<"To">>, EmailAddress},
                       {<<"Subject">>, im_common:ensure_binary(im_trans:t(Locale1, <<"report_email_subject">>, [ReportType]))}
                      ],
                      [],
                      Html2
                    })
   },
  Options = [
             {ssl, true},
             {no_mx_lookups, true},
             {relay, Relay},
             {username, Username},
             {password, Password},
             {auth, always}
            ],
  gen_smtp_client:send(Email, Options),
  ok.
