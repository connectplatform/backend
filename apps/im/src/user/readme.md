<h1>User</h1>

This folder contains modules related to user management, authentication, and related functionality.




<h2>im_acl.erl</h2>

The im_acl module is responsible for managing user roles and permissions in an IM application. It exports various functions to get roles, check if a user has a specific role or permission, and ensures that the permissions and roles are properly configured.

Here is a brief overview of the exported functions:

- <b>can_access/3</b>: Determines if a user can access a specific resource or perform a specific action based on their roles and permissions.
- <b>get_permissions/1</b>: Retrieves the permissions associated with a role.
- <b>spec/0</b>: Returns the spec for the standard worker.
- <b>get/1</b>: Gets user roles.
- <b>get_roles/1</b>: Gets roles for a specific user ID.
- <b>get_roles_for_perm/1</b>: Gets roles for a specific permission ID.
- <b>has_role/2</b>: Checks if a user has a specific role.
- <b>has_role_one_of/2</b>: Checks if a user has one of the specified roles.
- <b>has_perm/2</b>: Checks if a user has a specific permission.
- <b>has_perm_one_of/</b>2: Checks if a user has one of the specified permissions.
- <b>ensure_permissions/1</b>: Ensures permissions are properly configured.
- <b>ensure_roles/1</b>: Ensures roles are properly configured.
- <b>handle/3</b>: Handles various calls for getting roles, checking roles and permissions, and updating roles.

The module maintains a state record to store roles and the last role update timestamp. It also defines a role update interval constant (?ROLE_UPDATE_INTERVAL). The module is designed to work with a standard worker, handling calls for various operations related to roles and permissions.






<h2>im_auth.erl</h2>

This module is an Erlang module implementing authentication functionalities. It defines functions for handling social network authentication, phone verification, and user logout. Here's a brief description of the main functions:

- <b>authenticate/2</b>: Authenticates a user based on their credentials (username and password).
- <b>set_session/1</b>: Sets the session information for an authenticated user.
- <b>get_session/1</b>: Retrieves the session information for a user.
- <b>social/1</b>: Handles authentication using a social network (currently only supports Facebook) and returns an authentication token upon successful authentication.
- <b>request_verification/1</b>: Handles the request for phone number verification by sending a verification code via SMS to the provided phone number.
- <b>confirm_verification/1</b>: Validates the provided verification code against the stored code, and if valid, creates or finds an existing user token and returns it.
- <b>logout/3</b>: Logs out the user by removing the associated device and returns a logout response.
- <b>ensure_user_by_phone/1</b>: Ensures a user exists for the given phone number. If the user does not exist, it creates a new user record.
- <b>create_token/5</b>: Creates a new user token record.
- <b>find_token_by_id/1</b>: Retrieves a user token record by its ID.
- <b>create_or_find_token/4</b>: Creates a new user token or finds an existing one based on the provided device ID and user.

The module also includes several helper functions for processing phone verification, checking SMS code conformity, and managing token creation and retrieval.





<h2>im_department.erl</h2>

This module, im_department, defines functions to manage departments within the system. It includes the following exported functions:

- <b>get/2</b>: Retrieves a DepartmentResp record with the department's ref and a list of formatted departments.
- <b>list/1</b>: Lists departments based on the provided parameters (Skip, Limit, QueryString). It returns the total number of matching departments and a formatted list of departments.
- <b>c/1</b>: Creates a new department with the given Name if there isn't an existing department with the same name. It returns an error if the department name already exists.
- <b>r/1</b>: Retrieves a department by its Id. It returns a formatted department or an error if the department is not found.
- <b>u/2</b>: Updates a department's name by its Id. It returns an error if a department with the same name already exists.-
- <b>d/1</b>: Deletes a department by its Id. It returns an error if there is a directory associated with the department.
- <b>get_departments/0</b>: Retrieves all departments.
- <b>create_department/1</b>: Creates a new department with the provided data.
- <b>update_department/2</b>: Updates an existing department with the provided data.
- <b>delete_department/1</b>: Deletes a department with the specified ID.


Additionally, there is a format_department/1 function to format department records into DepartmentEntity records, which are then used in the responses of the exported functions.




<h2>im_device.erl</h2>

The Erlang module im_device is related to managing user devices and their associated push tokens. The module exports a number of functions for getting, adding, removing devices, and managing push tokens.

The exported functions are:

- <b>register_device/2</b>: Registers a new device for a user.
- <b>unregister_device/1</b>: Unregisters a device for a user.
- <b>get_devices/1</b>: Retrieves the devices associated with a user.
- <b>get/1</b>: Gets the devices associated with a specific user by their UserId.
- <b>find/2</b>: Finds a specific device by UserId and DeviceId.
- <b>add/5</b>: Adds a new device with the given UserId, Token, OS, DeviceId, and DeviceName.
- <b>remove/2</b>: Removes a device by UserId and DeviceId.
- <b>remove_all/1</b>: Removes all devices associated with a specific user by their UserId.
- <b>get_push_tokens/1</b>, <b>get_push_tokens/2</b>: Gets push tokens for a user, optionally filtered by DeviceId.
- <b>add_push_token/4</b>: Adds a push token to a user's device with the given UserId, DeviceId, TokenType, and Token.
- <b>remove_push_token/1</b>, remove_push_token/2: Removes a push token by Token or by UserId and Token.
- <b>filter_push_tokens/3</b>: Filters push tokens by UserId, OS, and Type.
- <b>format_devices/1</b>, <b>parse_devices/1</b>: Functions to format and parse devices.
- <b>format_device/1</b>, <b>parse_device/1</b>: Functions to format and parse a single device.
- <b>format_token/1</b>, <b>parse_token/1</b>: Functions to format and parse push tokens.

The module uses the im_roster_chat module for retrieving and updating the user information, the im_common module for formatting strings, and the im_logger module for logging information related to adding and removing devices and tokens. It also uses the ctail module for interacting with the data storage system.





<h2>im_device_api.erl</h2>

This Erlang module is called im_device_api. It is related to managing user devices and push tokens for notifications. The module exports three functions: get, remove, and add_token. These functions are used for:

- <b>register/2</b>: Registers a new device for a user using the provided data.
- <b>unregister/1</b>: Unregisters a device for a user using the provided device ID.
- <b>get</b>: Retrieves the devices associated with a specific user. It takes a Devices record with a ref field as input, along with the UserId. It returns a DevicesResp record containing a reference and a list of formatted devices.
- <b>remove</b>: Removes a device associated with a specific user. It takes a RemoveDevice record with a ref field and a deviceId field as input, along with the UserId. It returns a RemoveDeviceResp record containing a reference.
add_token: Adds a push token to a user's device for push notifications. It takes an AddPushToken record with a ref field, type field, and token field as input, along with the UserId. It returns an AddPushTokenResp record containing a reference.

The module uses the im_device and im_roster_chat modules for interacting with the data storage and managing user devices. It also uses the im_logger module for logging information related to adding tokens. The im_dto module is used for formatting device records.





<h2>im_directory.erl</h2>

This module manages the user directory. Functions include:

- <b>search/2</b>: Searches for users based on the provided query and options.
- <b>search_users/2</b>: Searches for users based on the provided query and options, with additional functionality for pagination.


This Erlang module is called im_directory. It seems to be related to managing and updating the information of users in a directory, such as their names, phone numbers, emails, and department IDs. The module exports several functions, such as is_turnedon, list, c, r, u, and d. These functions are used for:

<b>is_turnedon</b>: Determines if the directory is turned on or not, based on the environment configuration.
<b>list</b>: Lists directory entries based on the given input (Skip, Limit, and QueryString).
<b>c</b>: Creates a new directory entry with the given input (Name, Phone, Email, and DepartmentId).
<b>r</b>: Retrieves the directory entry for a specific phone number.
<b>u</b>: Updates the directory entry with the given input (UserId, Name, Phone, Email, and DepartmentId).
<b>d</b>: Blocks or unblocks a user in the directory based on the given input (UserId).
Other functions are also exported, such as <b>update_directory</b>, <b>spawn_contacts</b>, <b>update_directory_status</b>, <b>update_directory_avatar</b>, and <b>update_contacts</b>. These functions are used for updating the directory information, spawning contacts for a user, updating directory status, and updating directory avatars and contacts, respectively. The module uses the <b>ctail</b> and <b>ctail_mongo</b> modules for interacting with the data storage.

In addition, there are several helper functions within the module, such as <b>purify_phone</b>, <b>dirunit_to_contact</b>, <b>to_binary</b>, <b>make_id</b>, and <b>format_directory</b>. These functions are used for manipulating and formatting data related to the directory entries.






<h2>im_elastic_search.erl</h2>

This module provides utility functions for working with Elasticsearch. Functions include:

- <b>search/4</b>: Searches for documents in Elasticsearch based on the provided query, options, and index.
- <b>index/3</b>: Indexes a document in Elasticsearch based on the provided data and index.
- <b>delete/2</b>: Deletes a document from Elasticsearch based on the provided ID and index.


This Erlang code defines a function find_users_by_query/4 that takes four arguments - QueryString, FindOnlyVendors, UserIds, and UserId. The purpose of this function is to find users based on the given query string and other parameters. It performs the following tasks:

1. Checks if the current user has the "super_admin" role.
2. If FindOnlyVendors is true and the user is not an admin, it returns a permission denied error.
3. Otherwise, it processes the query string and filters the search results based on various conditions.
4. It then sends the search query to an Elasticsearch instance, which returns a list of matching users.
5. The function maps the returned Elasticsearch documents to a list of ContactEntity records and returns this list.

The <b>index_exists/1</b> function checks if an Elasticsearch index with the given name exists. It returns true if the index exists, and false otherwise.

The <b>parse_value/1</b> and <b>format_value/1</b> functions are helper functions that handle null and undefined values. The <b>parse_value/1</b> function converts a null value to undefined, while the format_value/1 function does the opposite, converting undefined to null.





<h2>im_locale.erl</h2>

The im_locale module is a part of the Connect Platform's backend code that handles user locale management for the instant messaging application. It provides functions to retrieve and set the locale for a user or a specific device.

Here's an overview of the module's functions and their purposes:

Exports:

The module exports several functions: get/1, get/2, set/2, and ensure_locale/1.
- <b>get/1</b> and <b>get/2</b>: These functions retrieve the locale for a given user ID or a specific device ID. If only the user ID is provided, the get/1 function calls the get/2 function with the device ID set to undefined.
The functions first attempt to retrieve the user's locale from the user state using the im_user_state:pids/1 and im_user_state:attr/2 functions. If the user's locale is not available in the user state, they attempt to retrieve the locale from the ctail database using the im_device_locale_lookup table and the device ID.
If the locale is not found in the ctail database, the functions return the default locale.
- <b>set/2</b>: This function is used to set the locale for a specific device ID. It takes the device ID and the locale as arguments.
The device ID is first converted to a binary using the im_common:ensure_binary/1 function. The locale is then passed to the ensure_locale/1 function to verify that it is a valid locale.
The function then stores the device ID and the locale in the ctail database using the im_device_locale_lookup table.
- <b>ensure_locale/1</b>: This function is a utility function that checks whether the provided locale is valid by verifying if it is a member of the list of supported locales (?LOCALES). If the locale is valid, it is returned; otherwise, the default locale (?DEFAULT_LOCALE) is returned.
- <b>get_locales/0</b>: Retrieves all available locales.
- <b>set_locale/2</b>: Sets a user's locale based on the provided locale code.

In summary, the im_locale module is responsible for managing user locales in the Connect Platform's instant messaging application. It provides functions for retrieving and setting locales for users and devices and handles the storage of locale information in the ctail database.





<h2>im_user_settings.erl</h2>

The im_user_settings module is part of the Connect Platform's backend code and is responsible for managing user settings in the instant messaging application. It handles user settings data retrieval, storage, and updating.

Here's an overview of the module's functions and their purposes:

Exports:

The module exports several functions: init/1, get/1, get_one/2, get_one/3, get/2, and set/2.
- <b>init/1</b>: This function initializes the user settings for a given user ID. It retrieves user settings data from the ctail database (a key-value store) and stores them in an ETS (Erlang Term Storage) table with the module name as the table identifier.
If the user settings data don't exist in the ETS table, it creates a new entry with an empty settings list.
- <b>get/1</b>:This function retrieves user settings data for a given user ID. If the user settings data are not available in the ETS table, it initializes them using the init/1 function.
It returns the user settings data as an im_setting record.
- <b>get_one/2 and get_one/3</b>: These functions retrieve a specific user setting value by key. They use the get/1 function to obtain the user settings data and then look up the value using the given key.
The get_one/3 function allows specifying a default value if the key is not present in the user settings data.
- <b>get/2</b>: This function is used to handle a UserSettings request, which includes a reference (ref) field. It retrieves the user settings data using the get/1 function and returns a UserSettingsResp record containing the formatted settings data and the reference.
- <b>set/2</b>: This function is used to update a user setting value. It takes a UserSetting request containing a reference (ref) and a setting to update. It retrieves the current user settings data from the ctail database and merges the new setting value with the existing data.
The updated settings data are stored back into the ctail database and the ETS table.
Finally, it returns a UserSettingResp record containing the updated formatted settings data and the reference.
- <b>merge_settings/2</b>: This function is a utility function that takes the current settings data and a new setting entry (key-value pair) as arguments. It merges the new entry into the existing settings data and returns the updated settings.
In summary, the im_user_settings module is responsible for managing user settings in the Connect Platform's instant messaging application. It provides functions for initializing, retrieving, and updating user settings data and handles the storage of settings data in both an ETS table and the ctail database.




<h2>im_user_state.erl</h2>

This module manages user states (e.g., online, offline, away). Functions include:

- <b>set_state/2</b>: Sets a user's state based on the provided state code.
- <b>get_state/1</b>: Retrieves a user's state.





<h2>im_user_transform.erl</h2>

This module provides utility functions for transforming user data. Functions include:

- <b>to_view/1</b>: Transforms a user's internal data representation into a view suitable for external consumption.

The im_user_transform module is part of the Connect Platform's backend code. It appears to handle the serialization and deserialization of user-related data in the instant messaging (IM) application. The module exports two main functions: serialize/1 and deserialize/1. Additionally, it implements a worker process using the im_std_worker behavior.

Here's a breakdown of the module's components:

Exports:

The module exports several functions: spec/0, handle/3, serialize/1, and deserialize/1:

These functions are the public interface for the module, allowing external code to call for serialization and deserialization of user-related data.

Both functions use the im_std_worker:call/2 function to delegate the actual work to the worker process, passing the operation (serialize or deserialize) and the data to be processed.

- <b>spec/0</b>: This function provides the worker process specification for the im_user_transform module, which is used when starting the worker.
The specification includes options for serializers and deserializers, which are lists of functions used to process user data during serialization and deserialization.

- <b>handle/3</b>: This function is the main callback for the im_std_worker behavior and is called when the worker process receives a message.

It handles different operations, such as init, call, and others. In the case of call, it processes the serialization and deserialization requests.

- <b>serialize/1</b> For serialization, it iterates over the serializers list and applies each function to the user data, returning the final serialized document.

- <b>deserialize/1</b> For deserialization, it iterates over the deserializers list and applies each function to the serialized document, returning the final deserialized user data.

In summary, this module is responsible for serializing and deserializing user-related data in the Connect Platform's instant messaging application. It does this by providing a worker process with a specific set of serialization and deserialization functions.





<h2>im_usr.erl</h2>

This module manages users. It includes several functions to get, update, validate, and handle user status, blocking, unblocking, and getting vendor-specific users.

Here's a brief summary of the main functions in the module:

- <b>register/1</b>: Registers a new user with the provided data.
- <b>get/3</b>: Retrieves user information based on the input parameters and returns a formatted response.
- <b>get_vendor_users/2</b>: Retrieves users related to a specific vendor, if the requesting user has the appropriate permissions.
- <b>update/3</b>: Updates a user's information and handles various validations and error cases. This function also triggers events and broadcasts messages to the appropriate users when a user's information is updated.
- <b>validate_user/1</b>: Validates a user's information, checking for empty names and invalid name lengths.
- <b>status/2</b>: Retrieves a user's status (online or offline) and last seen timestamp.
- <b>statuses/2</b>: Retrieves the statuses of all friends of a user.
- <b>get_status/1</b>: Determines the user's status (online or offline) based on their process IDs.
- <b>get_last_seen/1</b>: Retrieves the last seen timestamp for a user.
- <b>block_user_by_system/1</b>: Blocks a user from the system, setting their active status to false, and updates related contacts and devices.
- <b>unblock_user_by_system/1</b>: Unblocks a user in the system, setting their active status to true, and updates related contacts.

