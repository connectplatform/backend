-module(im_device_api).

-include("im_common.hrl").

-export([get/2, remove/2, add_token/2]).

get(#'Devices'{ref=Ref}, UserId) ->
  Devices = [im_dto:format_device(Device) || Device <- im_device:get(UserId)],
  {ok, #'DevicesResp'{ref=Ref, devices=Devices}}.

remove(#'RemoveDevice'{ref=Ref, deviceId=DeviceId}, UserId) ->
  im_roster_chat:execute(UserId, fun(User) ->
    {ok, im_device:remove_device(DeviceId, User)}
  end),
  #'RemoveDeviceResp'{ref=Ref}.

add_token(#'AddPushToken'{ref=Ref, type=Type, token=Token}, UserId) ->
  DeviceId = im_user_state:attr(UserId, deviceId),
  im_logger:info(UserId, "[Push] Adding token: ~p, type:  ~p, deviceId: ~p", [Token, Type, DeviceId]),
  im_device:add_push_token(UserId, DeviceId, Type, Token),
  #'AddPushTokenResp'{ref=Ref}.
