<h2>im_common.erl</h2>

The im_common.erl file is an Erlang module that provides a set of utility functions for formatting, parsing, and manipulating data. It includes functions to handle phone numbers, IDs, JSON files, and other data types. Here's a high-level overview of the module:

Exported functions: The module exports various functions that can be used by other modules.
1. format_id/1, parse_id/1: Functions to format and parse IDs.
2. format_integer/1: Formats an integer value.
3. format_utf8/1, length_utf8/1, format_utf8_array/1, crop_str/2, strip/1, trim/1, list_unique/1: Functions for handling UTF-8 strings.
4. format_phone/1, format_phone_with_code/2, format_lookup_value/1: Functions for formatting phone numbers.
5. format_price/2: Formats a price value.
6. ensure_* functions: Ensures that the given value is of the specified type (e.g., timestamp, list, binary, float, integer, or boolean).
7. parse_boolean/1: Parses a boolean value.
8. get_county_code/1, get_national_number/1, is_valid_phone/1: Functions for working with phone numbers.
9. get_binary_length/1, binarize_proplist/1: Functions to get the length of a binary value and to convert a proplist to binary.
10. proplist_to_tuple/1, tuple_to_proplist/1: Functions to convert between proplists and tuples.
11. read_json_file/1, json_to_mongo/1, mongo_to_json/1, read_lines/1, mongo_tuple_to_proplist/1: Functions for reading JSON files, converting JSON to MongoDB format, and reading lines from a file.
12. crypto_random_string/0, crypto_random_string/2: Functions for generating cryptographically secure random strings.
13. random_number/2, random_string/2: Functions for generating random numbers and strings.
14. timestamp_to_datetime/1, datetime_to_timestamp/1: Functions for converting between timestamps and datetime values.
15. base_url/0: A function that returns the base URL.
16. proplist_upsert/3: A function that performs an upsert operation on a proplist.

The module is organized into sections, with each section containing functions that serve a specific purpose. It is designed to be a utility module that provides a common set of functions that can be used throughout the application.

<br><br>

<h2>im.app.src</h2> 
This file defines the application specification for the Instant Messaging (IM) application. It includes the application's name, version, and dependencies (other applications required for the IM application to run). It also specifies the callback module (im) and default environment configuration.

<br><br>

<h2>im.erl</h2>
This file contains the main entry point for the IM application. It implements the application and supervisor behaviors. The <b>start/2</b> function initializes various components such as the worker pools, event handling, migration, and fixtures. The init/1 function sets up the application environment, including starting HTTP or HTTPS servers, initializing the translation system, and setting up various supervisors for different parts of the application. The <b>routes/0</b> function defines the HTTP routes for the application.

<br><br>

<h2>im_app_conf_watcher.erl</h2>
This file implements a worker process that watches the application configuration file for changes. The <b>spec/0</b> function defines the worker's specification. The <b>handle/3</b> function handles incoming messages, such as refreshing the configuration when a change is detected. The <b>get/0</b> and <b>get/1</b> functions provide a way to retrieve the current configuration or a specific part of it. The module includes various helper functions for locating and parsing the configuration file, getting the folder to watch, and extracting specific parts from the JSON configuration.

<br><br>

<h2>im_csr.erl</h2>
This <b>im_csr.erl</b> module handles the functionality of the customer support requests (CSRs) in a Connect-powered chat application. Here's a brief overview of the functions included in this module:

- <b>get_open/2</b>: Retrieves the open and dropped CSRs for a user.

- <b>room_csr/2</b>: Retrieves the CSRs associated with a room and the user's vendor ID.

- <b>create/2</b>: Creates a new CSR and assigns it to a room, then sends notifications to managers.

- <b>notificate_managers/1</b>: Sends a notification to managers when a new CSR is created.

- <b>send_ticket_created_push/2</b>: Sends a push notification to a user when a CSR is created.

- <b>grab/2</b>: Allows a user to grab a CSR if it is open or dropped, and the user's vendor ID matches the CSR's vendor ID.

- <b>drop/2</b>: Allows a user to drop a CSR if they grabbed it, and changes its status to dropped.

- <b>close/2</b>: Allows a user to close a CSR if they are either the reporter or the user who grabbed the CSR.

- <b>add_tag/2</b>: Adds a tag to a CSR if the user's vendor ID matches the CSR's vendor ID.

- <b>remove_tag/2</b>: Removes a tag from a CSR if the user's vendor ID matches the CSR's vendor ID.

<br><br>

<h2>im_dto.erl</h2>

This module is responsible for converting and formatting various data objects between their internal Erlang representations and their external representations, such as protocol buffers or JSON objects. 

It plays an essential role in ensuring that data is correctly converted and formatted between internal and external representations.

Some of the main objects that this module works with include <i>users</i>, <i>rooms</i>, <i>messages</i>, <i>updates</i>, <i>media</i>, and others.

Here's a summary of the key functions and their purposes:

- <b>format_user/1</b> and <b>parse_user/1</b>: Functions for converting between the internal <b><i>#im_usr{}</i></b> record and the external <b><i>#'UserEntity'{}</i></b> record. These functions are responsible for formatting and parsing user-related data.

- <b>format_room/1</b> and <b>parse_room/1</b>: Functions for converting between the internal <b><i>#im_grp{}</i></b> record and the external <b><i>#'RoomEntity'{}</i></b> record. These functions are responsible for formatting and parsing room-related data.

- <b>format_message/2, format_message/3</b>, and <b>parse_message/1</b>: Functions for converting between the internal <b><i>#im_msg{}</i></b> record and the external <b><i>#'MessageEntity'{}</i></b> record. These functions are responsible for formatting and parsing message-related data.

- <b>format_update/2, format_update/3</b>: Functions for converting between the internal <b><i>#im_update{}</i></b> record and the external <b><i>#'UpdateEntity'{}</i></b> record. These functions are responsible for formatting update-related data.

- <b>format_media/1</b> and <b>parse_media/1</b>: Functions for converting between the internal media tuple and the external <b><i>#'MediaEntity'{}</i></b> record. These functions are responsible for formatting and parsing media-related data.

- <b>format_geo/1</b> and <b>parse_geo/1</b>: Functions for converting between the internal geo tuple and the external <b><i>#'GeoEntity'{}</i></b> record. These functions are responsible for formatting and parsing geo-related data.

- <b>format_task/2</b> and <b>parse_task/1</b>: Functions for converting between the internal <b><i>#im_task{}</i></b> record and the external <b><i>#'TaskEntity'{}</i></b> record. These functions are responsible for formatting and parsing task-related data.

- <b>format_bot/1</b>: Function for converting the internal <b><i>#im_bot{}</i></b> record to the external <b><i>#'BotEntity'{}</i></b> record. This function is responsible for formatting bot-related data.

- <b>format_inline_keyboard_markup/1</b> and <b>parse_inline_keyboard_markup/1</b>: Functions for converting between the internal inline keyboard markup tuple and the external <b><i>#'InlineKeyboardMarkup'{}</i></b> record. These functions are responsible for formatting and parsing inline keyboard markup-related data.

- <b>format_reply_keyboard_markup/1</b> and <b>parse_reply_keyboard_markup/1</b>: Functions for converting between the internal reply keyboard markup tuple and the external <b><i>#'ReplyKeyboardMarkup'{}</i></b> record. These functions are responsible for formatting and parsing reply keyboard markup-related data.

- <b>format_remove_keyboard_markup/1</b> and <b>vparse_remove_keyboard_markup/1</b>: Functions for converting between the internal remove keyboard markup tuple and the external <b><i>#'RemoveKeyboardMarkup'{}</i></b> record. These functions are responsible for formatting and parsing remove keyboard markup-related data.

These functions mainly deal with formatting and parsing User, Room, Message, Update, ChatUpdate, Media, Geo, UserRole, and various Markup-related data structures. Here's a brief summary of the main functions in this part:

- <b>format_user/1</b>: Takes a user record and returns a <b><i>UserEntity</i></b> structure with formatted fields.

- <b>parse_user/1</b>: Takes a <b><i>UserEntity</i></b> structure and returns a user record with parsed fields.

- <b>format_roles/1</b> and <b>format_user_role/1</b>: Format user roles and permissions.

- <b>format_room/1</b> and <b>parse_room/1</b>: Format and parse <b><i>Room</i></b> data structures.

- <b>format_message/2</b> and <b>parse_message/1</b>: Format and parse <b><i>Message</i></b> data structures.

- <b>format_update/2, format_update/3</b> and <b>format_chat_update/2</b>: Format <b><i>Update</i></b> and <b><i>ChatUpdate</i></b> data structures.

- <b>format_media/1</b> and <b>parse_media/1</b>: Format and parse <b><i>Media</i></b> data structures.

- <b>format_geo/1</b> and <b>parse_geo/1</b>: Format and parse <b><i>Geo</i></b> data structures.

- <b>format_csr/1</b> and <b>parse_csr/1</b>: Format and parse <b><i>CSR</i></b> data structures.

- <b>format_call/1</b>: Format <b><i>Call</i></b> data structures.

- <b>format_task/2</b> and <b>parse_task/1</b>: Format and parse <b><i>Task</i></b> data structures.

- <b>format_bot/1</b>: Format <b><i>Bot</i></b> data structures.

- <b>format_inline_keyboard_markup/1</b> and <b>parse_inline_keyboard_markup/1</b>: Format and parse <b><i>InlineKeyboardMarkup</i></b> data structures.

- <b>format_reply_keyboard_markup/1</b> and <b>parse_reply_keyboard_markup/1</b>: Format and parse <b><i>ReplyKeyboardMarkup</i></b> data structures.

- <b>format_remove_keyboard_markup/1</b> and <b>parse_remove_keyboard_markup/1</b>: Format and parse <b><i>RemoveKeyboardMarkup</i></b> data structures.

- <b>format_setting/1, format_settings/1</b> and <b>parse_setting/1</b>: Format and parse <b><i>Setting</i></b> data structures.

- <b>format_feed_post_category/1</b> and <b>parse_feed_post_category/1</b>: Format and parse <b><i>FeedPostCategory</i></b> data structures.

- <b>format_feed_post/1</b> and <b>parse_feed_post/1</b>: Format and parse <b><i>FeedPost</i></b> data structures.

- <b>format_order/1</b>: Format <b><i>Order</i></b> data structures.

- <b>format_localized_store/1</b> and <b>parse_localized_store/1</b>: Format and parse <b><i>LocalizedStore</i></b> data structures.

- <b>get_localized_value/2</b>: Retrieve a localized value from a property list.

- <b>format_page/1</b>: Format <b><i>Page</i></b> data structures.

- <b>format_channel/1</b> and <b>parse_channel/1</b>: Format and parse <b><i>Channel</i></b> data structures.

- <b>format_location/1</b> and <b>parse_location/1</b>: Format and parse <b><i>Location</i></b> data structures.

- <b>format_channel_category/1</b> and <b>parse_channel_category/1</b>: Format and parse <b><i>ChannelCategory</i></b> data structures.

- <b>format_ogdata/1</b>: Format <b><i>OpenGraph</i></b> data structures.

- <b>format_device/1</b>: Format <b><i>Device</i></b> data structures.

These functions handle the conversion of data structures between the internal representation used by the application and the structures required for external communication, such as API responses.

This module also contains several functions to parse and format different types of data, such as <b>feed posts, payment info, orders, localized stores, pages, channels, locations, channel categories</b>, and <b>OG (Open Graph)</b> data. It also includes a function to <i>convert user data into JSON</i> format. Here are brief descriptions for the main functions:

- <b>parse_feed_post/1</b>: Parses a FeedPostEntity record and converts it into an im_feed_post record.
- <b>parse_payment_info/1</b>: Parses a FeedPostPaymentInfoEntity record and converts it into a paymentInfo tuple.
- <b>parse_payment_info_available_date/1</b>: Parses a FeedPostPaymentInfoDateEntity record and converts it into a paymentInfoDate tuple.
- <b>format_order/1</b>: Formats an im_order record and converts it into an OrderEntity record.
- <b>format_localized_store/1</b>: Formats an im_localized_store record and converts it into a LocalizedStoreEntity record.
- <b>parse_localized_store/1</b>: Parses a LocalizedStoreEntity record and converts it into an im_localized_store record.
- <b>get_localized_value/2</b>: Retrieves a localized value from a list of key-value pairs based on the provided locale.
- <b>format_page/1</b>: Formats an im_page record and converts it into a PageEntity record.
- <b>format_channel/1</b>: Formats a channel record and converts it into a ChannelEntity record.
- <b>parse_channel/1</b>: Parses a ChannelEntity record and converts it into a channel record.
- <b>format_location/1</b>: Formats a channel_location record and converts it into a LocationEntity record.
- <b>parse_location/1</b>: Parses a LocationEntity record and converts it into a channel_location record.
- <b>format_channel_category/1</b>: Formats a channel_category record and converts it into a ChannelCategoryEntity record.
- <b>parse_channel_category/1</b>: Parses a ChannelCategoryEntity record and converts it into a channel_category record.
- <b>format_ogdata/1</b>: Formats an im_ogdata record and converts it into an OgData record.
- <b>format_ogdata_media_list/1</b>: Formats a list of media records and converts them into a list of OgMedia records.
- <b>format_ogdata_media/1</b>: Formats a media tuple and converts it into an OgMedia record.
- <b>user_to_json/1</b>: Converts an im_usr record into JSON format.

<br><br>

<h2>im_event.erl</h2>

The <b>im_event.erl</b> module implements a simple event manager using the gen_event behavior in Erlang. This event manager handles specific events and carries out certain actions based on the events.

The primary functions in this module are:

- <b>initialize/0</b>: Initializes the event manager by starting the event manager process with a local name and adds the current module as an event handler.

- <b>fire/2</b>: Sends a notification to the event manager with the given event and data.

- <b>handle_event/2</b>: A callback function that is called whenever the event manager receives an event. It currently handles the ?USER_UPDATED_EVENT event and takes appropriate actions based on the user data. If the user is not a vendor, the user data is updated in the ElasticSearch. If the user has a data field, the function processes the user profile type, adds contacts, and handles circles based on the application configuration.

The remaining functions are callback functions required by the gen_event behavior:

- <b>init/1</b>: Initializes the event handler state.
- <b>handle_call/2</b>: Handles synchronous calls to the event handler.
- <b>code_change/3</b>: Handles code changes for hot code upgrades.
- <b>handle_info/2</b>: Handles any other messages that the event handler might receive.
- <b>terminate/2</b>: Called when the event handler is terminating.

This module essentially manages events and carries out the necessary actions based on the type of event it receives, focusing primarily on user updates.

<br><br>

<h2>im_init.erl</h2>

The <b>im_init.erl</b> module deals with the initialization and termination of a user session in the Connect Platform messenger backend. It has three main exported functions:

- <b>authenticate/2</b>: This function takes a token and a locale as arguments. It first ensures that the locale provided is valid using <b><i>im_locale:ensure_locale(Locale)</i></b>. Then, it tries to find the token by its ID using <b><i>im_auth:find_token_by_id(Token)</i></b>. If the token is found, it updates the locale of the token record, retrieves the user associated with the token, puts the token record back, and sets the locale for the device. It then returns <b><i>{ok, User, TokenToUse}</i></b>. If the token is not found, it returns <b><i>{error, token_not_found}</i></b>.

- <b>initialize/3</b>: This function takes a user record, a token record, and a version as arguments. If the deviceId in the token record is undefined, it returns {shutdown, 400}. Otherwise, it checks in the user state with the token record, starts the user's chat worker process, adds the device to the user's device list, initializes the user settings, and sends an authenticated message to itself. It returns an im_state record containing the user ID, user record, token record, and version.

- <b>terminate/1</b>: This function takes a user ID as an argument. It checks out the user state, and updates the user's last seen timestamp in the roster chat.

In summary, the <b>im_init.erl</b> module handles the authentication, initialization, and termination of user sessions for the Connect Platform messenger backend. It ensures that the user is properly authenticated and that their session information, including device details and user settings, is properly managed throughout their session.

<br><br>

<h2>im_trans.erl</h2>

The <b>im_trans.erl</b> module handles translations for the Connect Platform messenger backend. It provides functionality to initialize the translation data and perform translations based on the given keys and arguments. The module exports five main functions:

- <b>init/0</b>: This function initializes the translation data by reading the translations from a JSON file located in the priv/translations directory. It then creates an ETS table to store the translation data and inserts the data into the table.

- <b>t/1</b>: A function that takes a translation key as an argument and translates it using the fallback language (English by default). It calls the <b>t/2</b> function with the fallback language and the key.

- <b>t/2</b>: A function that can be called with either a language and a translation key or a translation key and a list of arguments. If called with a language and a key, it translates the key using the given language. If called with a key and a list of arguments, it translates the key using the fallback language and replaces placeholders in the translation with the provided arguments.

- <b>t/3</b>: A function that takes a language, a translation key, and a list of arguments as input. It translates the key using the given language and replaces placeholders in the translation with the provided arguments.

- <b>replace/2</b>: A helper function that replaces placeholders in the translation string with the provided arguments.

The module also includes several private helper functions, such as <b>ts/1, ensure_lang/1, langs/0</b>, and <b>data/0</b> to handle translation data retrieval and manipulation.

In summary, the <b>im_trans.erl</b> module provides translation functionality for the Connect Platform messenger backend, allowing translations based on keys and optional arguments. It reads the translations from a JSON file and stores them in an ETS table for efficient access.

<br><br>

<h2>im_worker_pool.erl</h2>

The <b>im_worker_pool.erl</b> module provides a worker pool implementation for the Connect Platform messenger backend. It allows you to manage and interact with workers for different modules and IDs. The module exports four main functions and a <b>spec/0</b> function to configure the worker pool:

- <b>spec/0</b>: This function returns a worker pool specification used to initialize the worker pool. It uses the <b>im_std_worker:spec/3</b> function to create a specification for a worker pool with the worker_pool ID, an empty list of children, and the <b>im_worker_pool:handle/3</b> callback function.

- <b>get/2</b>: This function takes a module and an ID as arguments, and retrieves the worker process's PID for the given module and ID from the worker pool. It calls the <b>im_std_worker:call/2</b> function with the worker_pool ID and a tuple containing the get atom, the module, and the ID.

- <b>ensure/3</b>: This function takes a module, an ID, and a list of child arguments as input. It ensures that a worker process exists for the given module and ID. If the worker process already exists, it returns the worker's PID; otherwise, it starts a new worker process and adds it to the worker pool.

- <b>exists/2</b>: This function takes a module and an ID as arguments, and checks whether a worker process exists for the given module and ID in the worker pool.

- <b>cleanup/2</b>: This function takes a module and an ID as input, and removes the worker process associated with the given module and ID from the worker pool.

The module also includes a <b>handle/3</b> callback function, which processes the various commands passed to the worker pool (get, ensure, exists, and cleanup). It handles the call type of the message and returns an appropriate result based on the command.

In summary, the <b>im_worker_pool.erl</b> module provides a worker pool implementation for the Connect Platform messenger backend. It enables you to manage and interact with worker processes for different modules and IDs, ensuring that worker processes exist when needed and cleaning them up when they are no longer required.