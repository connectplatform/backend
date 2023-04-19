-module(im_locale).

-include("im_common.hrl").

-export([get/1, get/2, set/2, ensure_locale/1]).

get(UserId) -> get(UserId, undefined).
get(UserId, DeviceId) ->
  case im_user_state:pids(UserId) of
    [] ->
      % DeviceId1 = case DeviceId of
      %   undefined ->
      %     case im_roster_chat:get(UserId) of
      %       undefined -> DeviceId;
      %       User ->
      %         AnyDevice = hd(im_device:get(UserId)),
      %         AnyDevice#im_device.id
      %     end;
      %   _ -> DeviceId
      % end,
      % case ctail:get(im_device_locale_lookup, DeviceId1) of
      case ctail:get(im_device_locale_lookup, DeviceId) of
        {ok, Lookup} -> Lookup#im_device_locale_lookup.locale;
        _            -> ?DEFAULT_LOCALE
      end;
    _  ->
      im_user_state:attr(UserId, locale)
  end.

set(DeviceId, Locale) ->
  ctail:put(#im_device_locale_lookup{
    id     = im_common:ensure_binary(DeviceId),
    locale = ensure_locale(Locale)
  }).

ensure_locale(Locale) ->
  case lists:member(Locale, ?LOCALES) of
    true  -> Locale;
    false -> ?DEFAULT_LOCALE
  end.
