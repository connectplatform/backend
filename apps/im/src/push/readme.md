<h1>Push Notifications</h1>


The following modules are responsible for sending push notifications to different platforms:

<b>im_apn_pool.erl</b> and <b>im_fcm_pool.erl</b> are two modules responsible for sending push notifications to iOS and Android devices, respectively. They both use the worker pool pattern to manage a pool of workers for sending push notifications.



<h2>im_apn_pool.erl</h2>

- <b>start_pool/0</b>: This function starts a worker pool with 10 workers for sending Apple Push Notifications (APNs) to iOS devices.
- <b>send/3</b>: This function is the interface for sending push notifications. It takes a user ID, a push token, and a message as arguments and enqueues the push notification request to the worker pool.

The gen_server implementation manages the state and handles the sending of push notifications using the APNs service. It logs the results of successful and failed deliveries and removes invalid tokens.



<h2>im_fcm_pool.erl</h2>

- <b>start_pool/0</b>: This function starts a worker pool with 10 workers for sending Firebase Cloud Messaging (FCM) notifications to Android devices.
- <b>send/3</b>: This function is the interface for sending push notifications. It takes a user ID, a push token, and a message as arguments and enqueues the push notification request to the worker pool.

The gen_server implementation manages the state and handles the sending of push notifications using the FCM service. It logs the results of successful and failed deliveries and removes invalid tokens.

Both modules use the <b>wpool</b> library to manage the worker pool, and their implementations are similar. They differ in their push notification services (APNs for iOS and FCM for Android) and the methods they use to send push notifications (<b>im_http:post/3</b> for APNs and <b>fcm:sync_push/3</b> for FCM).



<h2>im_push.erl</h2>

This is a general push notification module that provides an interface for sending push notifications to different platforms. The <b>send/3</b> and <b>send/4</b> functions are the main interfaces to send push notifications to users, taking a user or user ID, the platform (OS), and the message as arguments.



<h2>im_push_android.erl</h2>

This module is responsible for sending push notifications to Android devices using Firebase Cloud Messaging (FCM). The <b>send_message/2</b> function formats the message and sends the push notification to Android devices.



<h2>im_push_ios.erl</h2>

This module is responsible for sending push notifications to iOS devices using Apple Push Notification Service (APNs). The module provides multiple functions for sending different types of push notifications, such as <b>send_message/2</b>, <b>send_alert/3</b>, <b>send_badge/2</b>, and <b>send_voip/2</b>. These functions format the message and send the push notification to iOS devices.



<h2>im_push_web.erl</h2>

This module is responsible for sending web push notifications. The <b>send_message/2</b> and <b>send_alert/3</b> functions format the message and send the push notification to web platforms.

These modules work together to handle push notifications for different platforms. They use the <b>im_push.erl</b> module to enqueue the push notification request, and the corresponding platform-specific modules (<b>im_push_android.erl</b>, <b>im_push_ios.erl</b>, and <b>im_push_web.erl</b>) to handle the actual push notification sending for each platform.


<h2>im_push_tools</h2>

The im_push_tools.erl module provides helper functions to format push notifications for messages and alerts. It also provides a function to get the chat name based on the feed type and feed ID.

The main exported functions are:

1. <b>format_message_alert/2</b>: Formats the push notification message alert based on the message type, kind, and payload. This function first determines the feed type (chat or room) and then formats the alert based on the kind of message (call, normal, or system).

2. <b>get_chat_name/3</b>: Gets the chat name and thumbnail based on the feed type (chat or room) and feed ID. This function checks if the name is already available in the update or falls back to retrieving the name from the roster (for individual chats) or the room information (for group chats).

The module also has several private functions that help with the formatting of the alert messages:

- <b>format_alert_call/4</b>: Formats an alert for a missed call.
- <b>format_alert_normal/5</b>: Formats an alert for a normal message (text, audio, document, image, map, sticker, or video) for a chat or a room.
- <b>format_alert_system/5</b>: Formats an alert for a system message (e.g., user added to room, user kicked from room, user changed room avatar).
- <b>get_group_user_name/2</b>: Retrieves the group user name based on the user ID.
- <b>get_group_system_push_key/3</b>: Determines the key for a group system push notification.