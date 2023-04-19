<h2>im_call_api.erl</h2>
The im_call_api.erl module is an Erlang implementation of an API for handling call-related actions within an instant messaging application. This module works in conjunction with the im_call_worker.erl module you mentioned earlier. It defines various call-related functions such as getting calls, initializing a call, joining, declining, leaving a call, handling ICE candidates, and receiving media from other participants.

Here's a brief explanation of each exported function:

- <b>get/2</b>: This function retrieves a list of calls for a specific user. The result is a list of formatted call records.
- <b>get_one/2</b>: This function retrieves a single call record for a user by the call ID. If found, it returns a formatted call record; otherwise, it returns an error response.
- <b>init/3</b>: This function initializes a call for a user, given the feed type and feed ID. If a new call is successfully created, it returns the call ID; otherwise, it returns an error response.
- <b>join/2</b>: This function allows a user to join a call by its call ID. The response confirms the user has joined the call.
- <b>decline/2</b>: This function allows a user to decline a call invitation by its call ID. The response confirms the call was declined.
- <b>leave/2</b>: This function allows a user to leave an ongoing call by its call ID. The response confirms the user has left the call.
- <b>ice_candidate/2</b>: This function handles ICE (Interactive Connectivity Establishment) candidates for establishing peer-to-peer connections in real-time communication. It sends the ICE candidate to the specified call ID and sender. The response confirms the ICE candidate was sent.
- <b>receive_media_from/2</b>: This function sets up the media connection between the user and the target user in a call. It receives an SDP (Session Description Protocol) offer and sets up the connection accordingly. The response confirms the media connection was established.

This module uses functions from the im_call_sup and im_rtc_communicator modules to interact with the underlying call-related functionality and real-time communication infrastructure.




<h2>im_call_sup.erl</h2>

The im_call_sup.erl module is an Erlang implementation of a call supervisor for handling call-related actions and managing the lifecycle of call workers. It works together with the im_call_api.erl and im_call_worker.erl modules to manage calls in the instant messaging application. This module is responsible for starting and managing call worker processes, as well as interacting with other components such as the RTC (Real-Time Communication) server.

Here's a brief explanation of the exported functions:

- <b>start_link/0</b>: Starts the supervisor as a linked process with a local name.
- <b>init/1</b>: Initializes the supervisor with a simple one-for-one strategy and a child specification for im_call_worker.
- <b>init_call/3</b>: Initializes a call for a user given the feed type and feed ID. It either creates a new call or joins an existing one.
- <b>add_participant/2</b>: Adds a participant to a call given the call ID and user ID.
- <b>remove_participant/2</b>: Removes a participant from a call given the call ID and user ID.
- <b>destroy/1</b>: Destroys a call given the call ID.
- <b>decline/2</b>: Declines a call invitation given the user ID and call ID.
- <b>cleanup_call/2</b>: Cleans up call-related resources given the roster feed ID and call ID.
- <b>is_missed/2</b>: Determines whether a call was missed by a user given the call ID and user ID.
- <b>find_user_call/2</b>: Finds a call for a user given the user ID and call ID.

Internally, the module uses im_worker_pool to manage call worker processes, the im_ets module for storage, and interacts with the im_rtc_communicator module for managing real-time communication. The module also maintains call information in an #call_info{} record, which includes the roster feed ID associated with the call.

<h2>im_call_worker.erl</h2>

This Erlang code is related to voice/video calling functions in a messaging application. It defines a gen_server named im_call_worker. The gen_server behavior is an OTP behavior that abstracts a server in a client-server architecture. It simplifies the implementation of servers by providing a standard set of functions to handle client requests, manage state, and handle errors.

The state of the server is represented by a record named state, which contains various fields like rosterFeedId, callId, initiatorId, timers, call participants, and call status.

The start_link/3 function is used to start the gen_server, and the init/1 function initializes the state of the server. The handle_call/3, handle_cast/2, and handle_info/2 functions handle different types of messages sent to the server.

The code handles various aspects of a call, such as:

- Initiating a call: When the server is initialized, it sends call invitations to the target users through push notifications and updates their chat state.
- Adding a user to the call: The handle_call({add, UserId}, _, State) function handles adding a user to the call. It updates the server state, persists the changes, and broadcasts notifications to other participants.
- Removing a user from the call: The handle_call({remove, UserId}, _, State) function handles removing a user from the call. It checks if the call has ended, updates the server state, and sends notifications to other participants.
- Declining a call: The handle_call({decline, UserId}, _, State) function handles a user declining a call. It updates the server state and checks if the call has ended.
- Heartbeat monitoring: The handle_info(heartbeat, State) function periodically checks the call status to see if it has ended or timed out. It updates the server state accordingly and sends appropriate notifications.
- Send "Call Ended" push notifications to all users involved in the call, update their user state, and broadcast the "CallEnded" event.
- Persist call information, such as call duration, status, and participation details, for all users involved in the call.
- Update chat updates for each call.
- Send a message to the chat indicating that a call took place.
- Clean up the call information after it has ended.
- Implement the code_change/3 function, which is a standard callback for hot code upgrades in Erlang.

Define helper functions to:

- Determine if a call has ended.
- Convert milliseconds to Firebase Cloud Messaging (FCM) format.
- Get the status of a user in a call.
- Get a list of users in a roster chat.

The code also handles call termination, where it logs the termination event and updates the call status accordingly.


<h2>im_rtc_communicator.erl</h2>

The im_rtc_communicator.erl module is responsible for handling communication between the RTC server and the clients. It contains various functions for sending and receiving messages between the clients and the RTC server. These messages facilitate tasks such as initiating, joining, leaving, and cleaning up calls.

Here's a brief explanation of the main functions:

- <b>init(UserId, CallId, Participants)</b>: This function initializes a call with the given user ID, call ID, and participants. It sends an "InitCall" message to the RTC server.

- <b>join(UserId, CallId)</b>: This function allows a user to join an ongoing call. It sends a "Join" message to the RTC server.

- <b>leave(UserId, CallId)</b>: This function allows a user to leave an ongoing call. It sends a "Leave" message to the RTC server.

- <b>cleanup(CallId) and cleanup(UserId, CallId)</b>: These functions clean up resources for a call or a user in a call. They send either a "CleanupCall" or "CleanupUser" message to the RTC server.

- <b>receive_media_from(UserId, CallId, SdpOffer, TargetUserId)</b>: This function handles receiving media from a user, with the given SDP offer and target user ID. It sends a "ReceiveVideoFrom" message to the RTC server.

- <b>send_ice_candidate(UserId, CallId, IceCandidateEntity, SenderUserId)</b>: This function sends an ICE candidate to the RTC server for a given user, call, and sender. It sends an "OnIceCandidate" message to the RTC server.

- <b>send(#'RtcMessage'{ref = Ref, name = Name, jsonData = JsonData, userId = UserId})</b>: This function sends a message to the RTC server over HTTP. It makes a POST request to the RTC server URL.

- <b>receive_message(#'RtcMessage'{ref = Ref, name = Name, jsonData = JsonData, userId = UserId})</b>: This function receives a message from the RTC server and processes it by calling the handle_received_message/3 function.

- <b>handle_received_message(MessageName, Data, UserId)</b>: This function handles messages received from the RTC server based on the message name, data, and user ID. It processes various types of messages, such as "CallCreated," "UserAdded," "UserRemoved," "IceCandidate," and "ReceiveVideoAnswer."

The module also contains helper functions to convert ICE candidates between entity and JSON formats, and a broadcast_to_user/2 function to send messages to a specific user.

Overall, the im_rtc_communicator.erl module facilitates communication between clients and the RTC server, enabling features like call initiation, joining, leaving, and media exchange.