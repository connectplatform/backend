-module(im_sms).

-include("im_common.hrl").

-define(SMS_PROVIDER_TWILIO, im_sms_twilio).
-define(SMS_PROVIDER_ALPHASMS, im_sms_alpha).
-define(SMS_PROVIDER_NEXMO, im_sms_nexmo).
-define(SMS_PROVIDER_THAILAND, im_sms_thailand).

-export([send/3]).

send(CountryCode, PhoneNumber, Text) ->
  do_send(CountryCode, PhoneNumber, Text).

do_send(CountryCode, PhoneNumber, Text) ->
  Providers = get_providers(CountryCode),
  MainProvider = element(1, Providers),
  AlternativeProvider = element(2, Providers),

  TextToSend = binary_to_list(unicode:characters_to_binary(Text)),

  im_logger:info(undefined, "[SMS] Country code: ~p, providers: ~p", [CountryCode, Providers]),

  Res = MainProvider:send(CountryCode, PhoneNumber, TextToSend),
  case Res of
    ok ->
      im_logger:info(undefined, "[SMS] Successfully sent with main provider", []),
      ok;
    Error ->
      im_logger:error(undefined, "[SMS] Failed to send with main provider: ~p", [Error]),
      case AlternativeProvider:send(CountryCode, PhoneNumber, TextToSend) of
        ok -> im_logger:info(undefined, "[SMS] Successfully sent with alternative provider", []), ok;
        AltError -> im_logger:error(undefined, "[SMS] Failed to send with alternative provider: ~p", [AltError])
      end
  end.

get_providers(CountryCode) ->
  Default = {?SMS_PROVIDER_TWILIO, ?SMS_PROVIDER_ALPHASMS},
  proplists:get_value(CountryCode, shema(), Default).

shema() ->
  [
    {66, {?SMS_PROVIDER_THAILAND, ?SMS_PROVIDER_TWILIO}},
    {380, {?SMS_PROVIDER_ALPHASMS, ?SMS_PROVIDER_TWILIO}}
  ].
