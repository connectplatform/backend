-module(im_sms_alpha_worker).
-behaviour(gen_server).

-include_lib("xmerl/include/xmerl.hrl").

-define(ALPHASMS_URL, "http://alphasms.ua/api/xml.php").
-define(ALPHASMS_SMS_TYPE, "0").
-define(XMERL_PROLOG, "<?xml version=\"1.0\" encoding=\"utf-8\"?>").
-define(CHECK_TIMEOUT, 700).

-define(STATUS_CODE_SCHEDULED, "100").
-define(STATUS_CODE_ENROUTE, "101").
-define(STATUS_CODE_DELIVERED, "102").
-define(STATUS_CODE_ACCEPTED, "106").
-define(STATUS_CODE_SENDING, "110").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/2]).

-record(state, {
  alphaname :: list(),
  package_key :: list()
}).

start_link(Alphaname, PackageKey) ->
  gen_server:start_link(?MODULE, [Alphaname, PackageKey], []).

init([Alphaname, PackageKey]) ->
  {ok, #state{
    alphaname = Alphaname,
    package_key = PackageKey
  }}.

handle_call({send, PhoneNumber, Code}, _, State = #state{alphaname = Alphaname, package_key = PackageKey}) ->
  Body = lists:flatten(
    xmerl:export_simple(
      [
        #xmlElement{
          name = package,
          attributes = [
            #xmlAttribute{
              name = key,
              value = PackageKey
            }
          ],
          content = [
            #xmlElement{
              name = message,
              content = [
                #xmlElement{
                  name = msg,
                  attributes = [
                    #xmlAttribute{
                      name = recipient, value = PhoneNumber
                    },
                    #xmlAttribute{
                      name = sender,
                      value = Alphaname
                    },
                    #xmlAttribute{
                      name = type,
                      value = ?ALPHASMS_SMS_TYPE
                    }
                  ],
                  content = [
                    #xmlText{
                      value = Code
                    }
                  ]
                }
              ]
            }
          ]
        }
      ],
      xmerl_xml,
      [
        {prolog, ?XMERL_PROLOG}
      ]
    )
  ),

  HttpResponse = httpc:request(post, {
    ?ALPHASMS_URL,
    [],
    "text/xml",
    Body
  }, [], []),

  {ok, {{_, HttpCode, _}, _, Data}} = HttpResponse,

  Res = case HttpCode of
    200 ->
      {Xml, _} = xmerl_scan:string(Data),
      [RespElement | _] = Xml#xmlElement.content,
      case RespElement#xmlElement.name of
        message ->
          [MsgContent | _] = RespElement#xmlElement.content,
          MsgAttrs = MsgContent#xmlElement.attributes,
          SmsId = [MsgAttr#xmlAttribute.value || MsgAttr <- MsgAttrs, MsgAttr#xmlAttribute.name =:= sms_id],
          check_sms(SmsId, PackageKey);
        error ->
          [ErrorElement | _] = Xml#xmlElement.content,
          {error, "AlphaSMS Error Code: " ++ ErrorElement#xmlText.value};
        _ -> {error, "HTTP unknown error"}
      end;
    _ ->
      {error, "HTTP Error with code: " ++ HttpCode}
  end,

  {reply, Res, State};
handle_call(_Request = _, _, State = #state{}) -> {reply, ok, State}.

handle_cast(_Request = {}, State = #state{}) -> {noreply, State}.

handle_info(_Info, State = #state{}) -> {noreply, State}.

terminate(_, _State = #state{}) -> ok.

code_change(_OldVsn, State = #state{}, _Extra) -> {ok, State}.

check_sms(SmsId, PackageKey) ->
  Body = lists:flatten(
    xmerl:export_simple(
      [
        #xmlElement{
          name = package,
          attributes = [
            #xmlAttribute{
              name = key,
              value = PackageKey
            }
          ],
          content = [
            #xmlElement{
              name = status,
              content = [
                #xmlElement{
                  name = msg,
                  attributes = [
                    #xmlAttribute{
                      name = sms_id, value = SmsId
                    }
                  ]
                }
              ]
            }
          ]
        }
      ],
      xmerl_xml,
      [
        {prolog, ?XMERL_PROLOG}
      ]
    )
  ),

  HttpResponse = httpc:request(post, {
    ?ALPHASMS_URL,
    [],
    "text/xml",
    Body
  }, [], []),

  {ok, {{_, HttpCode, _}, _, Data}} = HttpResponse,

  case HttpCode of
    200 ->
      {Xml, _} = xmerl_scan:string(Data),
      [RespElement | _] = Xml#xmlElement.content,
      [MsgContent | _] = RespElement#xmlElement.content,
      [StatusContent | _] = MsgContent#xmlElement.content,

      % im_logger:debug(undefined, "StatusCode: ~p~n", [StatusContent#xmlText.value]),

      case StatusContent#xmlText.value of
        ?STATUS_CODE_DELIVERED -> ok;
        ?STATUS_CODE_ENROUTE   -> ok;
        StatusCode when StatusCode =:= ?STATUS_CODE_ACCEPTED;
          StatusCode =:= ?STATUS_CODE_SCHEDULED;
          StatusCode =:= ?STATUS_CODE_SENDING ->
          timer:sleep(?CHECK_TIMEOUT),
          check_sms(SmsId, PackageKey);
        StatusCode -> {error, "AlphaSMS Error Code: " ++ StatusCode}
      end;
    _ ->
      {error, "HTTP Error with code: " ++ HttpCode}
  end.
