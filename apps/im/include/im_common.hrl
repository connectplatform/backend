-ifndef(IM_COMMON_HRL).
-define(IM_COMMON_HRL, true).

-include_lib("smoothie/include/sm.hrl").

-include("generated/IM.hrl").
-include("generated/IM_const.hrl").

-include("im_entities.hrl").

-record(im_state, {userId, user, token, version}).

-define(SYS_USER_ID, {<<0,0,0,0,0,0,0,0,0,0,6,102>>}).

-define(LOCALES, [<<"uk">>, <<"ru">>, <<"en">>]).
-define(DEFAULT_LOCALE, <<"en">>).

-define(DEFAULT_REGION_CODE, <<"US">>).
-define(DEFAULT_COUNTRY_CODE, <<"+1">>).
-define(MINIMUM_NATIONAL_NUMBER_LENGTH, 6).

-define(APPLE_USER_PHONE, <<"+18452288552">>).
-define(APPLE_USER_SMS_CODE, "313371").

-define(SMS_SENDER_PROVIDER_TYPE_MAIN, 1).
-define(SMS_SENDER_PROVIDER_TYPE_ALTERNATIVE, 2).

-define(EVENT_MANAGER, event_manager).
-define(USER_UPDATED_EVENT, user_updated_event).

-endif.
