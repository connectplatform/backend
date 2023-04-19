-module(im_feedback).

-include("im_common.hrl").

-define(TOPIC_SIZE, 250).
-define(BODY_SIZE, 1000).

-export([add/2]).

add(#'AddFeedback'{name=Name, email=Email, topic=Topic, message=Message}, undefined) ->
  case (Name =:= undefined) or (Email =:= undefined) of
    true ->
      {ok, #'ErrorResp'{ code=?ERROR_CODE_FEEDBACK_FIELDS_INVALID,
                         ref=0, messageParams=[],
                         messageType= <<"feedback.field.email.name.required">>,
                         message=im_common:format_utf8(im_trans:t(<<"feedback.field.email.name.required">>))}};
    false ->
      case ((length(unicode:characters_to_list(Message, utf8)) > ?BODY_SIZE) or (length(unicode:characters_to_list(Topic, utf8)) > ?TOPIC_SIZE)) of
        true ->
          {ok, #'ErrorResp'{code=?ERROR_CODE_FIELD_LENGTH_NOT_VALID,
                            ref=0, messageParams=[],
                            messageType= <<"feedback.field.length.not.valid">>,
                            message=im_common:format_utf8(im_trans:t(<<"feedback.field.length.not.valid">>))}};
        false ->
          ctail:put(#im_feedback{
                       id      = ctail:next_id(),
                       name    = Name,
                       email   = Email,
                       client  = ?FEEDBACK_CLIENT_TYPE_RADIUS_IM,
                       topic   = Topic,
                       message = Message,
                       createdAt = sm:now()
                      }),

          {ok, #'AddFeedbackResp'{}}
      end
  end;
add(#'AddFeedback'{client=Client, topic=Topic, message=Message, email=Email}, #im_usr{name=Name, phone=Phone, id=UserId}) ->
  case (
        (Message =:= undefined) or
        (Topic =:= undefined) or
        (Email =:= undefined) or
        (Message =:= "") or
        (Topic =:= "") or
        (Email =:= "")
   ) of
    true ->
      {ok, #'ErrorResp'{code=?ERROR_CODE_FIELD_LENGTH_NOT_VALID,
                        ref=0, messageParams=[],
                        messageType= <<"feedback.field.length.not.valid">>,
                        message=im_common:format_utf8(im_trans:t(<<"feedback.field.length.not.valid">>))}};
    false ->
      case (
            (length(unicode:characters_to_list(Message, utf8)) > ?BODY_SIZE) or
            (length(unicode:characters_to_list(Topic, utf8)) > ?TOPIC_SIZE)
       ) of
        true ->
          {ok, #'ErrorResp'{code=?ERROR_CODE_FIELD_LENGTH_NOT_VALID,
                            ref=0, messageParams=[],
                            messageType= <<"feedback.field.length.not.valid">>,
                            message=im_common:format_utf8(im_trans:t(<<"feedback.field.length.not.valid">>))}};
        false ->
          case Client of
            0 ->
              {ok, #'ErrorResp'{ code=?ERROR_CODE_FEEDBACK_FIELDS_INVALID,
                                 ref=0, messageParams=[],
                                 messageType= <<"feedback.field.client.required">>,
                                 message=im_common:format_utf8(im_trans:t(<<"feedback.field.client.required">>))}};
            _ ->
              ctail:put(#im_feedback{
                           id      = ctail:next_id(),
                           userId  = UserId,
                           email   = Email,
                           name    = Name,
                           phone   = Phone,
                           client  = Client,
                           topic   = Topic,
                           message = Message,
                           createdAt = sm:now()
                          }),

              {ok, #'AddFeedbackResp'{}}
          end
      end
  end.
