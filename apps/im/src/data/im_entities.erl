-module(im_entities).

-include_lib("cocktail/include/ctail.hrl").
-include("im_entities.hrl").

-export([meta/0]).

meta() ->
  #schema{name=im, tables=[
    #table{name=im_usr,                    fields=record_info(fields, im_usr)},
    #table{name=im_usr_role,               fields=record_info(fields, im_usr_role)},
    #table{name=im_usr_perm,               fields=record_info(fields, im_usr_perm)},
    #table{name=im_usr_token,              fields=record_info(fields, im_usr_token)},
    #table{name=im_code,                   fields=record_info(fields, im_code)},
    #table{name=im_contact,                fields=record_info(fields, im_contact)},
    #table{name=im_lookup_phone,           fields=record_info(fields, im_lookup_phone)},
    #table{name=im_lookup_facebook,        fields=record_info(fields, im_lookup_facebook)},
    #table{name=im_add_contacts_task,      fields=record_info(fields, im_add_contacts_task)},
    #table{name=im_usr_phone,              fields=record_info(fields, im_usr_phone)},
    #table{name=im_feedback,               fields=record_info(fields, im_feedback)},
    #table{name=im_report,                 fields=record_info(fields, im_report)},

    #table{name=im_usr,                    fields=record_info(fields, im_usr)},
    #table{name=im_msg_star,               fields=record_info(fields, im_msg_star)},
    #table{name=im_grp,                    fields=record_info(fields, im_grp)},
    #table{name=im_msg,                    fields=record_info(fields, im_msg)},
    #table{name=im_update,                 fields=record_info(fields, im_update)},
    #table{name=im_muc_history_marker,     fields=record_info(fields, im_muc_history_marker)},
    #table{name=im_directory,              fields=record_info(fields, im_directory)},
    #table{name=im_department,             fields=record_info(fields, im_department)},
    #table{name=im_bot,                    fields=record_info(fields, im_bot)},
    #table{name=im_bot_server_msg,         fields=record_info(fields, im_bot_server_msg)},
    #table{name=im_bot_user_lookup,        fields=record_info(fields, im_bot_user_lookup)},
    #table{name=im_bot_username_lookup,    fields=record_info(fields, im_bot_username_lookup)},
    #table{name=im_device_locale_lookup,   fields=record_info(fields, im_device_locale_lookup)},
    #table{name=im_device_push_token_lookup,fields=record_info(fields, im_device_push_token_lookup)},
    #table{name=im_feed_post,              fields=record_info(fields, im_feed_post)},
    #table{name=im_feed_post_tag,          fields=record_info(fields, im_feed_post_tag)},
    #table{name=im_feed_post_category,     fields=record_info(fields, im_feed_post_category)},
    #table{name=im_localized_store,        fields=record_info(fields, im_localized_store)},
    #table{name=im_csr,                    fields=record_info(fields, im_csr)},
    #table{name=im_task,                   fields=record_info(fields, im_task)},
    #table{name=im_file,                   fields=record_info(fields, im_file)},
    #table{name=im_lookup_file,            fields=record_info(fields, im_lookup_file)},
    #table{name=im_order,                  fields=record_info(fields, im_order)},
    #table{name=im_order_serial_lookup,    fields=record_info(fields, im_order_serial_lookup)},
    #table{name=im_order_avail_date_reserve,fields=record_info(fields, im_order_avail_date_reserve)},
    #table{name=im_call,                   fields=record_info(fields, im_call)},
    #table{name=im_call_user_lookup,       fields=record_info(fields, im_call_user_lookup)},
    #table{name=im_like,                   fields=record_info(fields, im_like)},
    #table{name=im_like_count,             fields=record_info(fields, im_like_count)},
    #table{name=im_page,                   fields=record_info(fields, im_page)},
    #table{name=im_setting,                fields=record_info(fields, im_setting)},
    #table{name=im_migration,              fields=record_info(fields, im_migration)},
    #table{name=im_ogdata,                 fields=record_info(fields, im_ogdata)},
    #table{name=im_workflow,               fields=record_info(fields, im_workflow)},
    #table{name=im_workflow_state,         fields=record_info(fields, im_workflow_state)},
    #table{name=im_workflow_version,       fields=record_info(fields, im_workflow_version)},

    #table{name=channel,                   fields=record_info(fields, channel)},
    #table{name=channel_post_link,         fields=record_info(fields, channel_post_link)},
    #table{name=channel_post,              fields=record_info(fields, channel_post)},
    #table{name=channel_location,          fields=record_info(fields, channel_location)},
    #table{name=channel_category,          fields=record_info(fields, channel_category)},
    #table{name=channel_post_share_lookup, fields=record_info(fields, channel_post_share_lookup)},
    #table{name=channel_post_like_lookup,  fields=record_info(fields, channel_post_like_lookup)},
    #table{name=channel_post_thread,       fields=record_info(fields, channel_post_thread)},
    #table{name=channel_user_sub_lookup,   fields=record_info(fields, channel_user_sub_lookup)},
    #table{name=user_channel_sub_lookup,   fields=record_info(fields, user_channel_sub_lookup)},
    #table{name=user_channel_lookup,       fields=record_info(fields, user_channel_lookup)}
  ]}.
