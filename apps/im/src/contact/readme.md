<h2>im_contact.erl</h2>

This Erlang module, <b>im_contact</b>, manages user contact information in an instant messaging application. It provides functions to find, format, update, upsert (insert or update), and generate new contacts from user information. It also handles actions related to contacts when a user's information is changed, when a user registers, and retrieves friend IDs and system labels for contacts.

The main functions in this module are:

- <b>update_contacts_when_user_changed/1</b>: Updates contacts when the user's information has changed. It takes a user record as input and updates the lookup and contacts accordingly.
- <b>add_contacts_after_user_registered/1</b>: Adds contacts for the new user after they register. It takes a user record as input and adds contacts based on certain rules and conditions.
- <b>find/2</b>: Finds a contact between two users by their IDs. Returns an error if not found.
- <b>get_friends_ids/1</b>: Retrieves friend IDs of a user.
- <b>generate_new_contact_from_user/2</b>: Generates a new contact record based on the provided user records.
- <b>format/1</b>: Formats a user or contact record into a ContactEntity.
- <b>update/4</b>: Updates the contact record with a new name and custom labels for a given user and target user.
- <b>upsert/1</b>: Inserts or updates a contact record, handling lookup and chat updates accordingly.
- <b>update_contacts_when_user_changed/2</b>: This function updates the contact information when a user's information has changed. It checks if the user's phone number is already associated with a contact and if so, updates the contact information in the lookup. It then calls the update_contacts_when_user_changed/3 function, which recursively updates the contact information for each contact in the lookup.
- <b>update_contacts_when_user_changed/3</b>: This recursive function iterates over the list of contact IDs and updates their contact information based on the specified name change strategy. If the name change strategy is set to 'strategy_from_user', it uses the updated user's name for the contact. If the strategy is set to 'strategy_from_contact', it retains the existing contact name unless it is undefined, in which case it uses the updated user's name. The function also updates other contact attributes such as photo, thumbnail, departmentId, and bio.
- <b>add_contacts_after_user_registered/1</b>: This function is called after a user has registered. It adds contacts to the user's contact list based on the application's configuration rules. The function first gets the list of rules for adding contacts from the configuration, and then applies the rules by performing queries on the user database. The results are merged to create a list of users to be added as contacts, and these contacts are then added to the user's contact list.
- <b>find/2</b>: This function finds a contact by its user ID and target user ID. It iterates over the list of contacts associated with the given user ID and returns the contact with the matching target user ID.
- <b>get/2</b>: This function retrieves a contact's information from the lookup by providing the user ID and the contact ID. It uses the user ID to find the list of contacts associated with that user and then iterates over that list to find the contact with the specified contact ID. If the contact is found, it returns the contact information; otherwise, it returns an error.
- <b>get_all/1</b>: This function retrieves all contacts associated with a given user ID. It simply takes the user ID as input and returns the list of contacts associated with that user.
- <b>delete/2</b>: This function deletes a contact by its user ID and contact ID. It first retrieves the list of contacts associated with the user ID and then filters out the contact with the specified contact ID. After that, it updates the contact list in the lookup with the filtered list.
- <b>delete_all/1</b>: This function deletes all contacts associated with a given user ID. It takes the user ID as input and removes the associated contact list from the lookup.
- <b>search/3</b>: This function searches for contacts by providing the user ID, query string, and search options. It first retrieves the list of contacts associated with the user ID, then filters the list based on the query string and search options. The search options can include parameters such as search by name, department, or bio. After filtering the list, it returns the matching contacts.
- <b>import/2</b>: This function imports a list of contacts for a given user ID. It takes the user ID and a list of contacts as input, and it updates the contact list in the lookup by appending the imported contacts to the existing contacts associated with the user ID. It then returns the updated contact list.
- <b>export/1</b>: This function exports all contacts associated with a given user ID. It takes the user ID as input, retrieves the list of contacts associated with that user, and returns the contact list in a format suitable for exporting, such as CSV or JSON.
- <b>update/3</b>: This function updates a contact's information by providing the user ID, contact ID, and updated contact data. It first retrieves the list of contacts associated with the user ID and then iterates over that list to find the contact with the specified contact ID. If the contact is found, it updates the contact information with the new data provided and then updates the contact list in the lookup.

In addition to these core functions, the contact manager module may also include utility functions and validation functions to handle input validation, data formatting, and other common tasks.

By implementing these functions, the contact manager module would provide a complete set of operations to manage contacts for multiple users, including creating, updating, deleting, searching, importing, and exporting contacts.

This module could be integrated into a larger application, such as a web-based contact management system, by using a web framework, REST API, or other communication methods to expose the functions to end-users. The module could also be adapted to work with various storage backends, such as databases or cloud storage services, to provide persistent storage for the contact data.



<h2>im_contact_api.erl</h2>

The im_contact_api.erl file is an Erlang module that provides an API for managing contacts in an instant messaging application. It includes functions to add, delete, block, unblock, update, and sync contacts, as well as add contacts to a spam list.

Here is a brief description of each function:

- <b>upload/3</b>: Uploads contacts and adds a task to the contact queue.
- <b>add/2</b>: Adds a contact to the user's contact list, sends a system message to both users informing them of the action, and returns a response containing the added contact's information.
- <b>delete/2</b>: Deletes a contact from the user's contact list and returns a response with the deleted contact's information.
- <b>block/2</b>: Blocks a contact and returns a response with the blocked contact's information.
- <b>unblock/2</b>: Unblocks a contact and returns a response with the unblocked contact's information.
- <b>add_to_spam/2</b>: Adds a contact to the user's spam list and returns a response with the contact's information.
- <b>update/2</b>: Updates a contact's information, such as name and labels, and returns a response with the updated contact's information.
- <b>get/2</b>: Retrieves a contact's information and returns a response with the contact's information. If the contact is not found, it either returns an error response stating that the user is not found, or it generates a new contact from the user's roster and returns the contact's information.
- <b>sync_contacts/2</b>: Syncs the user's contacts based on the provided sync time. It retrieves the user's contacts that have been updated since the sync time and returns a response containing the server time and the updated contacts.
Each function takes different input parameters and operates on specific data structures. The module uses finite state machines (FSMs) to manage contact relationships, and messages are sent between users using the im_message module.

In summary, this Erlang module provides a comprehensive API for managing contacts in an instant messaging application, allowing users to add, delete, update, and sync their contacts while handling various contact-related actions such as blocking, unblocking, and adding contacts to a spam list.



<h2>im_contact_fsm.erl</h2>

This Erlang module, <b>im_contact_fsm</b>, implements a finite state machine for managing contact relationships between users in an instant messaging application. The module uses gen_statem behavior, which is a generic state machine behavior in Erlang.

The code defines several events related to contact management, such as adding, deleting, blocking, unblocking, and adding users to the spam list.

The module exports several functions for initialization, starting the process, and public APIs for adding, deleting, blocking, unblocking, and adding users to the spam list.

The state machine has several custom states, like unknown_contact, pending_invitation, friend, blocked, i_am_blocked, added_to_spam, and i_am_added_to_spam. These states represent different relationships between users.

The state functions handle events and perform actions based on the current state and received event. For example, when the state is unknown_contact and the event is to add a user, the action_add_to_contact/2 function is called. Similarly, different actions are executed based on the state and event combinations.

The actions perform updates to the contact relationship between users, update the state, and send appropriate messages to the users involved. The actions include:

- <b>action_add_to_contact/2</b>: Adds a user to the contact list.
- <b>action_user_add_to_contact_you/1</b>: Handles the case where a user adds you to their contact list.
- <b>action_delete_contact/2</b>: Deletes a user from the contact list.
- <b>action_user_deleted_me/1</b>: Handles the case where a user deletes you from their contact list.
- <b>action_user_added_to_spam_me/1</b>: This function is called when the current user is added to the spam list by another user. It updates the current contact's status to CONTACT_STATUS_I_AM_ADDED_TO_SPAM and returns the next state and updated data.
- <b>handle_event/3</b>: This function is a generic event handler for unknown or unhandled events. It logs the event and returns an error response for call type events or simply keeps the state for cast type events.

Action functions are called by the state functions to execute specific actions based on the event received. Actions may update the contact information, send messages to users, or interact with the roster chat. After performing the action, they usually return the next state and the updated data.

In general, this Erlang module (im_contact_fsm.erl) implements a finite state machine for managing contact relationships between users in a chat application. It defines events and actions for adding, deleting, blocking, unblocking, and marking contacts as spam. The module uses the gen_statem behavior to handle the state transitions and event handling.


<h2>im_contact_merge.erl</h2>

This Erlang module, im_contact_merge.erl, is responsible for processing and merging contacts from an external source (e.g., importing contacts from the phone's address book) with the contacts stored in the application's database. The module defines the following functions:

- <b>process/2</b>: This function takes two arguments: UserId and Contacts. It retrieves the user's contacts from the database (DbContacts) and then merges them with the provided Contacts. After merging, it iterates through the merged contacts, making sure the user's contact is not in the list (EnsureWithoutMeContacts). For each contact in the list, it checks and updates contact information, adds or updates lookup information, and persists the contacts in the database. Finally, it returns the list of merged contacts.
- <b>merge/3</b>: This function is a helper function for merging the provided Contacts list with the DbContacts list. It ensures that duplicate contacts are not added, and updates contact information when necessary (e.g., when a contact status changes from pending to friend). It takes three arguments: a list of contacts to merge, a list of contacts from the database, and an accumulator for storing the merged contacts.
- <b>find_contact/3</b>: This function searches for a given contact within a list of contacts. It takes three arguments: the contact to search for, the list of contacts to search within, and an accumulator for storing the search result. The function returns the found contact or an empty list if the contact is not found.
- <b>persist_contact/2</b>: This function is responsible for persisting the contact information in the database. It takes two arguments: UserId and a contact record Contact. If the contact doesn't have an ID, it generates a new ID, sets the feed_id, and assigns the current timestamp to the createdAt and updatedAt fields. If the contact already has an ID, it just updates the updatedAt field with the current timestamp. In both cases, it returns the updated contact record.

To summarize, im_contact_merge.erl is a module that provides functionality for merging and processing contacts from an external source with contacts stored in the application's database. The main function, process/2, takes a user ID and a list of contacts, merges them with existing contacts in the database, updates contact and lookup information as necessary, and persists the updated contacts. It uses helper functions merge/3, find_contact/3, and persist_contact/2 to achieve its tasks.


<h2>im_contact_queue.erl</h2>

The im_contact_queue.erl module defines a worker that processes contact addition tasks in a queue. The module exports functions to add tasks to the queue, initialize and handle the worker, and process the contact addition tasks.

Here's a brief explanation of the main functions and their responsibilities:

- <b>spec/0</b>: Returns the worker specification for the contact queue.
- <b>add/1</b>: Adds a contact addition task to the queue. It takes a task as an argument.
- <b>handle/3</b>: Handles different messages sent to the worker process, such as initializing the worker, processing tasks in the queue, and adding tasks.
- <b>process/1</b>: Processes a contact addition task. It takes a task as an argument. It first calls process_contacts/1 to process the contacts, and if an error occurs, it returns {error, retrie} to retry the task later.
- <b>process_contacts/1</b>: Processes the contacts in the task. It takes a task record as an argument. It converts the binary data in the task to a list of contact entities, parses and merges the contacts, and sends a contact resolved event.
- <b>parse_upload_contacts/2</b>: Parses the uploaded contacts, filters out duplicates, and returns a list of unique contacts. It takes a list of contact entities and a user ID as arguments.
- <b>get_unique_phone_list/2</b>: Returns a sorted list of unique phone numbers. It takes a list of contacts and an accumulator as arguments.
- <b>remove_duplicate/3</b>: Removes duplicate contacts from the list. It takes a list of unique phone numbers, a list of contacts, and an accumulator as arguments.
- <b>apportion_contact/2</b>: Creates a list of contacts from a given contact entity. It takes a contact entity and a user ID as arguments. It formats the phone numbers of the contact entity and creates an im_contact record for each phone number.
- <b>contact_resolved_event/1</b>: Sends a contact resolved event after processing the contacts. It takes a tuple containing the user ID, device ID, and resolved user IDs as arguments. It adds new contacts to the user's roster and broadcasts a ContactResolvedEvent message.

In summary, the im_contact_queue.erl module defines a worker that processes contact addition tasks in a queue. The worker processes tasks by parsing and merging the contacts, and then sending a contact resolved event. The module exports functions to add tasks to the queue, initialize and handle the worker, and process the contact addition tasks.