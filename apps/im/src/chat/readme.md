<h1>Messaging Module</h1>

The "Messaging" module is a collection of Erlang files that work together to create a robust and efficient messaging system for chat applications. This module handles various aspects of messaging such as chat updates, circles, messages, pointers, retrieval, and rooms. The following files are included in this module:

- <b>im_chatupdate.erl</b>: This file handles chat updates, including sending and receiving updates when there are new messages or changes in the chat.
- <b>im_chatupdate_api.erl</b>: The API module for the chat updates, providing functions to interact with the chat updates in a user-friendly way.
- <b>im_circle.erl</b>: This file manages circles, which are groups of users that can communicate with each other. Circles can be used to implement features like group chats or custom user lists.
- <b>im_message.erl</b>: This file defines the main message structure, handling the creation and manipulation of messages in the system.
- <b>im_message_api.erl</b>: The API module for messages, providing functions for sending, receiving, and processing messages.
- <b>im_pointer.erl</b>: This file deals with pointers, which are used to keep track of the user's position in the chat history. Pointers help users navigate through previous messages and maintain a sense of continuity in the conversation.
- <b>im_retrieve.erl</b>: This file is responsible for retrieving messages and chat updates from the backend storage, allowing users to load historical messages and stay up-to-date with the conversation.
- <b>im_retrieve_api.erl</b>: The API module for message retrieval, providing functions for fetching messages and chat updates.
- <b>im_room.erl</b>: This file manages chat rooms, which are virtual spaces where users can send and receive messages. Rooms can be public or private, with access controlled by the room's creator.

Together, these files create a comprehensive messaging system that supports various features such as group chats, private messages, and chat history retrieval. The Messaging module ensures efficient communication between users, providing a seamless chat experience in a Connect-powered application.

<h2>im_chatupdate.erl</h2>

This Erlang code defines a module im_chatupdate, which is responsible for managing chat updates in an instant messaging application. The main functionalities provided by this module are:

1. Get chat updates for a specific user.
2. Mark a chat update as read for a user.
3. Calculate the total number of unread chat updates for a user.
4. Add or update chat updates.
5. Find and ensure the existence of a chat update.
6. Delete a chat update.
7. Convert between message feed and roster representation for chat updates.
8. Perform quick update and quick find operations on chat updates.
9. Wait for a chat update to be available.
10. Send chat update to users.

This module makes use of several other modules like im_common, im_roster_chat, im_roster, im_push_ios, im_user_state, and im_dto. It defines an im_chat_update record to store chat update information, such as timestamp, delivered status, seen status, unread count, and other properties. The functions in this module perform various operations on the chat updates, like filtering, mapping, updating, and more.

In summary, this module handles the logic related to managing and manipulating chat updates in an instant messaging module.



<h2>im_chatupdate_api.erl</h2>

The im_chatupdate_api Erlang module provides an API for interacting with chat updates in an instant messaging application. It exports three main functions:

- <b>get/2</b>: This function takes a ChatUpdates record and a UserId as arguments. It retrieves chat updates for the given user based on the provided feedType, feedId, and syncTime. The returned chat updates are formatted and wrapped in a ChatUpdatesResp record.
- <b>mark_as_read/2</b>: This function takes a MarkAsRead record and a UserId as arguments. It marks the chat update corresponding to the given feedType and feedId as read for the specified user. It returns a MarkAsReadResp record with a reference to the original request.
- <b>delete/2</b>: This function takes a DeleteChat record and a UserId as arguments. It deletes the chat update corresponding to the given feedType and feedId for the specified user. It returns a DeleteChatResp record with a reference to the original request.

In summary, the im_chatupdate_api module provides an API for accessing and manipulating chat updates in an instant messaging application. It relies on the im_chatupdate module for the core functionality and serves as an interface for external entities to interact with chat updates.



<h2>im_circle.erl</h2>

This Erlang code is a module (im_circle) that manages "circles" in a chat system. A circle is a group of users, and the module provides functionality to create and manage these circles based on certain parameters. The main exported function is get/2, which takes two arguments: a record of type GetCircle and a user ID.

The get/2 function does the following:

1. Parses the input parameters.
2. Retrieves existing circles based on the input parameters.
3. If the ForceCreate flag is set to true or no existing circles are found, it creates a new circle. Otherwise, it updates the existing circle with the new members and parameters.
4. Sends a circle update to the bot supervisor.
5. Returns the circle's room ID and a chat update object.

The module also includes several helper functions for filtering, parsing, and formatting circle parameters. These functions help manage the circles' unique attributes and ensure that the circle parameters are correctly stored and compared.

Here's a brief summary of the helper functions:

- <b>filter_circles/2</b>: Filters chat updates based on matching unique parameters.
- <b>parse_params/1</b>: Parses a parameter string into a sorted list of key-value pairs.
- <b>format_params/1</b>: Formats a list of parameters into a URL-encoded query string.
- <b>sort_params/1</b>: Sorts a list of parameters based on their keys.



<h2>im_message.erl</h2>

im_message.erl is an Erlang module that handles the core functionality of messages in an instant messaging application. The module is responsible for sending, editing, deleting, starring, and retrieving messages. It also deals with message updates, visibility, and notifications.

Here are the main exported functions:

- <b>get_updates/4</b>: Retrieves a list of message updates for a user within a given range (Top, Stop, Count).
- <b>send/4</b> and <b>send/5</b>: Sends a message to a chat or room. The message can be a regular user message or a system message.
- <b>send_from_sys_user/3</b>: Sends a system message from a system user.
- <b>send_sys_msg/5</b> and <b>send_sys_msg/7</b>: Sends a system message with additional parameters such as recipient and recipient-only restrictions.
- <b>edit/2</b>: Edits an existing message.
- <b>delete/4</b>: Deletes a message.
- <b>star/3</b>: Stars a message for a user.
- <b>get_starred/3</b>: Retrieves a user's starred messages.
- <b>is_visible/2</b>: Determines if a message is visible to a user.
- <b>wipe_message/1</b>: Wipes a message's content.
- <b>get_top_message/4</b>: Retrieves the top message in a chat or room.
- <b>target_user_ids/3</b>: Retrieves a list of user IDs who are targets of a message in a chat or room.
- <b>create_update/2</b> and <b>create_update/3</b>: Creates a new message update.
- <b>markup_user/1</b>: Marks up a user.
- <b>parse_msg_meta/1</b>: Parses message metadata.
- <b>edit/2</b>: This function edits a message based on the input parameters. It first checks if the message exists and if the original message was sent by the provided UserId. If the conditions are met, it updates the message attributes and sends an update to the targeted users.
- <b>delete/4</b>: This function deletes a message or a list of messages. It updates the deletedByOwner attribute for messages owned by the UserId and the deletedBy attribute for messages not owned by the UserId. After updating the messages, it sends an update to the targeted users.
- <b>star/3</b>: This function either stars or unstars messages for a given UserId. It retrieves the current list of starred messages, updates the list based on the provided Star boolean, and then stores the updated list.
- <b>get_starred/3</b>: This function retrieves the list of starred messages for a given UserId, applying the provided skip and limit parameters for pagination.
- <b>is_visible/2</b>: This function checks if a message is visible for a given UserId, considering the deleted status of the message and whether the message is intended for the recipient.
- <b>markup_user/1</b>: This function creates a markup for a given UserId.
- <b>parse_msg_meta/1</b>: This function parses message metadata, such as mentions in the message payload.
- <b>send_push/4</b>: This function sends a push notification to the target user if certain conditions are met, such as the message not being a missed call and the user having notifications enabled. It sends notifications using iOS, Android, and Web push services.
- <b>get_user_info/1</b>: This function retrieves the user's name and thumbnail based on the provided UserId.
- <b>wipe_message/1</b>: This function replaces a message with a system notification indicating that the message has been deleted.
- <b>get_top_message/4</b>: This function retrieves the top visible message from a conversation, excluding the user's own messages if specified by the ExcludeMine parameter. It checks the visibility of the message based on deletion status and recipient restrictions.
- <b>target_user_ids/3</b>: This function returns the list of target user Ids based on the feed type (either chat or room) and the current user's UserId. In the case of a chat feed, it returns the parsed FeedId, and in the case of a room feed, it returns a list of group members excluding the current user.
- <b>edit/2</b>: This function edits a message based on the input parameters. It first checks if the message exists and if the original message was sent by the provided UserId. If the conditions are met, it updates the message attributes and sends an update to the targeted users.
- <b>delete/4</b>: This function deletes a message or a list of messages. It updates the deletedByOwner attribute for messages owned by the UserId and the deletedBy attribute for messages not owned by the UserId. After updating the messages, it sends an update to the targeted users.
- <b>star/3</b>: This function either stars or unstars messages for a given UserId. It retrieves the current list of starred messages, updates the list based on the provided Star boolean, and then stores the updated list.
- <b>get_starred/3</b>: This function retrieves the list of starred messages for a given UserId, applying the provided skip and limit parameters for pagination.
- <b>is_visible/2</b>: This function checks if a message is visible for a given UserId, considering the deleted status of the message and whether the message is intended for the recipient.
- <b>markup_user/1</b>: This function creates a markup for a given UserId.
- <b>parse_msg_meta/1</b>: This function parses message metadata, such as mentions in the message payload.
- <b>send_push/4</b>: This function sends a push notification to the target user if certain conditions are met, such as the message not being a missed call and the user having notifications enabled. It sends notifications using iOS, Android, and Web push services.
- <b>get_user_info/1</b>: This function retrieves the user's name and thumbnail based on the provided UserId.
- <b>wipe_message/1</b>: This function replaces a message with a system notification indicating that the message has been deleted.
- <b>get_top_message/4</b>: This function retrieves the top visible message from a conversation, excluding the user's own messages if specified by the ExcludeMine parameter. It checks the visibility of the message based on deletion status and recipient restrictions.
- <b>target_user_ids/3</b>: This function returns the list of target user Ids based on the feed type (either chat or room) and the current user's UserId. In the case of a chat feed, it returns the parsed FeedId, and in the case of a room feed, it returns a list of group members excluding the current user.

These functions work together to handle various messaging functionalities such as editing, deleting, starring messages, and sending push notifications.

The module also contains several internal helper functions that deal with sending messages to chats or rooms, creating updates, updating feeds, sending push notifications, and checking for blocked users. These functions are used to handle the core functionality in the exported functions and are not meant to be used directly by other modules.

Please note that these descriptions provide an overview of the functionality in the im_message.erl module. For more specific information on how each function works, the source code and comments within the code will be beneficial.



<h2>im_message_api.erl</h2>

The im_message_api.erl module handles various message-related operations in the chat application. It exports several functions that correspond to actions such as getting updates, sending, editing, deleting, starring, and reporting messages, as well as notifying others when a user is typing.

- <b>get_updates/2</b>: Retrieves message updates based on the user's UserId, Top, Stop, and Count parameters. The function formats the updates using the im_dto:format_update/2 function and returns an UpdatesResp record with the result.
- <b>send/2</b>: Sends a message to a specific FeedType (chat or room) and FeedId. It handles various error cases, such as invalid feed type, blocked users, deleted groups, or users who are not members of a group. It returns a MessageResp record with the formatted message or an ErrorResp record with the appropriate error code and message.
- <b>send_error_to_resp/2</b>: Helper function to convert error reasons into ErrorResp records with the appropriate error code and message.
- <b>edit/2</b>: Edits a message and returns an EditMessageResp record with the edited message, or an ErrorResp record if the operation fails.
- <b>delete/2</b>: Deletes messages with the specified Ids from the chat or room feed. It returns a DeleteResp record with the deleted Ids or an ErrorResp record if the operation fails.
- <b>star/2</b>: Stars or unstars messages with the specified MessageIds based on the Star parameter (true or false). The function returns a StarResp record.
- <b>get_starred/2</b>: Retrieves the starred messages for a user with the provided UserId, Skip, and Limit parameters. It formats the messages using im_dto:format_message/2 and returns a GetStarredResp record containing the formatted messages and the total number of messages.
- <b>typing/2</b>: Handles user typing notifications in a chat or room feed. It broadcasts a Typing message to other users in the feed, indicating that the current user (UserId) is typing. Depending on the FeedType (chat or room), the function calls im_user_state:broadcast/3 or lists:foreach/2 to send the typing notification to the appropriate users.
- <b>report_message/2</b>: Reports a message with the specified MessageId. It sets the status of existing reports related to the message to "old" and wipes the reported message using im_message:wipe_message/1. The function returns a ReportMessageResp record.

Overall, the im_message_api.erl module provides a set of functions that allow you to interact with the chat application's messaging system. These functions handle the main messaging operations, such as sending, editing, deleting, starring, and reporting messages, as well as notifying other users about typing events and retrieving updates for messages. By using these functions, you can build and manage a chat application's messaging system, enabling users to interact with each other through messages in different feeds (chats or rooms). Additionally, the module takes care of error handling and provides appropriate responses in case of errors, ensuring a smooth user experience.



<h2>im_pointer.erl</h2>

This Erlang module (im_pointer.erl) is designed to handle and update message pointers, specifically for delivered and seen messages in both chat and room feeds. It exports three main functions:
- <b>update/2</b>: Updates the delivered and seen pointers for a given user and a specific feed (either a chat or a room). It takes a 'Pointer' record and a UserId as arguments, and returns a 'PointerResp' record with a reference.
- <b>delivered/4</b>: Processes the update of delivered pointers for messages. This function takes four arguments: FeedType, FeedId, UserId, and Delivered. Depending on the FeedType (chat or room), it updates the delivered pointer for all target users in the feed and sends the update.
- <b>seen/4</b>: Processes the update of seen pointers for messages. It takes four arguments: FeedType, FeedId, UserId, and Seen. Depending on the FeedType (chat or room), it updates the seen pointer for the given user or all target users in the feed and sends the update.

The module also includes several helper functions:
- <b>get_chatupdate/2</b>: Retrieves the chat update for a given UserId and UpdateId.
- <b>put_chatupdate/2</b>: Updates the chat update for a given UserId and the im_chat_update record.
- <b>seen_inner/4</b>: Handles the inner logic of updating seen pointers for chat and system user messages.
- <b>calc_unread_count/4</b>: Calculates the number of unread messages for a user based on the Seen value and the message chain.

Overall, this module is responsible for managing the message pointers for users in chat and room feeds, ensuring that the delivered and seen pointers are updated correctly. By doing so, it helps maintain the accurate state of chat messages and keeps track of the users' unread messages count.

In summary, this module is an essential part of a chat application, as it ensures that the application can accurately show the status of messages (e.g., delivered, seen) and handle unread message counts for users in different feeds. By using Erlang's powerful pattern matching and recursion, this module efficiently processes updates and propagates them throughout the system.



<h2>im_retrieve.erl</h2>
The im_retrieve.erl module is responsible for fetching messages in a chat application based on specific criteria. It provides a single exported function, get_messages/7, which retrieves messages from chat or room feeds with a specified direction, count, top, and stop parameters.

Here is a brief explanation of the function parameters:

- <b>UserId</b>: The user ID for whom messages are being fetched.
- <b>FeedType</b>: The type of feed (either chat or room) from which to fetch messages.
- <b>FeedId</b>: The identifier of the chat or room feed.
- <b>Count</b>: The maximum number of messages to retrieve.
- <b>Top</b>: The starting message ID for the retrieval.
- <b>Stop</b>: The stopping message ID for the retrieval.
- <b>Direction</b>: The direction in which messages should be retrieved, either up or down.

The function first sanitizes the inputs and provides default values if needed. Then, based on the FeedType, it performs different actions:

For chat feeds, the get_messages/7 function filters the retrieved messages by checking if they are visible to the user (UserId) using the im_message:is_visible/2 function. It fetches messages from chat feeds by calling the im_roster_chat:retrieve/7 function.

For room feeds, the function first checks if the user is a member of the room. If the user is a member, it fetches messages from the room feed by calling the im_roster_muc:retrieve/7 function, filtering the retrieved messages by checking if they are visible to the user (UserId) using the im_message:is_visible/2 function and if the message creation timestamp is greater than or equal to the room's time marker. The time marker is fetched using the im_room:get_marker_time/2 function.

If the user is not a member of the room, the function returns an error not_a_member.

Finally, the function processes the retrieved messages:

If an error not_a_member is encountered, it returns the same error.
Otherwise, it returns the retrieved messages as a tuple {ok, Messages}.
In summary, the im_retrieve.erl module handles message retrieval for chat and room feeds based on the specified criteria, ensuring that only visible messages are returned to the user. It also checks for membership in room feeds before retrieving messages.


<h2>im_retrieve_api.erl</h2>

The im_retrieve_api.erl module defines a single function, retrieve/2, which is responsible for processing message retrieval requests and returning the appropriate response.

The function takes two arguments:
- A record of type #'Retrieve'{}
- A user ID (UserId)

The #'Retrieve' record contains the following fields:

- <b>ref</b>: A reference value for the request
- <b>feedType</b>: The type of feed (chat or room) to retrieve messages from
- <b>count</b>: The number of messages to retrieve
- <b>feedId</b>: The ID of the feed (chat or room) to retrieve messages from
- <b>top</b>: The ID of the top message in the retrieval range
- <b>stop</b>: The ID of the message to stop the retrieval at
- <b>direction</b>: The direction of retrieval (up or down)

The retrieve/2 function starts by calling the im_retrieve:get_messages/7 function with the provided arguments, which retrieves the messages based on the specified criteria.

The result of the im_retrieve:get_messages/7 call is then pattern-matched:

If the result is {error, not_a_member}, an #'ErrorResp' record with the reference Ref and the error code ?ERROR_CODE_PERMISSION_DENIED is returned. This indicates that the user does not have permission to retrieve messages from the specified room.
If the result is {ok, Messages}, the messages are formatted using the im_dto:format_message/2 function, and an #'RetrieveResp' record with the reference Ref and the formatted messages is returned. This indicates that the messages have been successfully retrieved according to the specified criteria.

In summary, the im_retrieve_api.erl module handles the API calls for retrieving messages for a specific user in a chat or room. It processes the message retrieval request, checks for necessary permissions, retrieves the messages, formats the messages, and returns the appropriate response to the caller.


<h2>im_room.erl</h2>

This Erlang file defines a module <b>im_room</b> that provides functions to create, manage, and manipulate chat rooms. Here is a simplified explanation of the main functions:
- <b>create/2</b>: Creates a new chat room and returns a formatted room response.
- <b>new/2</b>: Initializes a new room with the given members and admins, and adds it to the roster.
- <b>topic/2</b>: Changes the topic of a chat room, but only if the user is an admin.
- <b>picture/2</b>: Changes the picture and thumbnail of a chat room, but only if the user is an admin.
- <b>kick/2</b>: Removes specified members from a chat room, but only if the user is an admin.
- <b>add/2</b>: Adds new members to a chat room, but only if the user is already a member of the room.
- <b>role/2</b>: Changes the role of specified members in a chat room (either to a member or an admin), but only if the user is an admin.
- <b>quit/2</b>: Allows a user to quit a chat room, as long as there is at least one other admin in the room.
- <b>delete/2</b>: Deletes a chat room, but only if the user is an admin and there are no other members.

These functions utilize several internal and helper functions to manage chat room data, perform updates, and send system messages to inform users of changes.