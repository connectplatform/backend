-module(im_user_settings).
-compile({no_auto_import, [get/1]}).

-include("im_common.hrl").
-export([init/1, get/1, get_one/2, get_one/3]).
-export([get/2, set/2]).

-define(ETS_NAME, ?MODULE).

init(UserId) ->
  case im_ets:exists(?ETS_NAME, UserId) =:= false of
    true ->
      Settings = case ctail:get(im_setting, UserId) of
        {ok, S} ->
          Set = S#im_setting.settings,
          BinSet = im_common:binarize_proplist(Set),
          S#im_setting{settings=BinSet};
        {error, _} ->
          #im_setting{id=UserId, settings=[]}
      end,
      im_ets:put(?ETS_NAME, {UserId, #im_user_settings{settings=Settings}});
    false -> skip
  end.

get(UserId) ->
  case im_ets:exists(?ETS_NAME, UserId) of
    true -> skip;
    false -> init(UserId)
  end,
  Data = case im_ets:get_one(?ETS_NAME, UserId) of
    {UserId, Data1} -> Data1;
    _           -> #im_user_settings{}
  end,
  Data#im_user_settings.settings.

get_one(UserId, Key) ->
  get_one(UserId, Key, undefined).
get_one(UserId, Key, Default) ->
  Set = get(UserId),
  proplists:get_value(Key, Set#im_setting.settings, Default).

get(#'UserSettings'{ref=Ref}, UserId) ->
  #'UserSettingsResp'{ref=Ref, settings=im_dto:format_settings(get(UserId))}.

set(#'UserSetting'{ref=Ref, setting=Setting}, UserId) ->
  Set = case ctail:get(im_setting, UserId) of
    {ok, Settings} -> Settings;
    {error, _}     -> #im_setting{id=UserId, settings=[]}
  end,

  SettingEntry = im_dto:parse_setting(Setting),
  Merged = merge_settings(Set#im_setting.settings, SettingEntry),
  NewSettings = Set#im_setting{settings=Merged},
  ctail:put(NewSettings),
  im_ets:put(?ETS_NAME, {UserId, #im_user_settings{settings=NewSettings}}),

  #'UserSettingResp'{ref=Ref, settings=im_dto:format_settings(NewSettings)}.

merge_settings(All, {K,V}) ->
  K1 = im_common:ensure_binary(K),
  V1 = im_common:ensure_binary(V),

  All1 = im_common:binarize_proplist(All),
  All2 = case proplists:is_defined(K1, All1) of
    false -> All1;
    true  -> proplists:delete(K1, All1)
  end,

  im_common:binarize_proplist([{K1,V1}|All2]).
