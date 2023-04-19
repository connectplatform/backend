<h1>Data Handlers</h1>

This folder contains various data handlers for a Connect-powered application backend.

<br /><br />

<h2>im_workflow</h2>

The im_workflow.erl code is responsible for handling workflows. Here's a brief breakdown of the key parts of the code:

- <b>version_id/2</b>: A function that takes a name and version as arguments and returns a tuple containing the formatted UTF-8 name and version.
- <b>state_id/2</b>: A function that takes a name and an EntityId as arguments and returns a tuple containing the formatted UTF-8 name and the formatted EntityId.
- <b>parse_definition/2</b>: A function that takes an im_workflow record and a StateVersion as arguments and returns a definition record containing the workflow's attributes parsed from a JSON string.
- <b>parse_definition_step/1</b>: A function that takes a step represented as a struct (map) and returns a step record with the parsed properties.
- <b>find_definition_step/2</b>: A function that takes a definition record and a step name as arguments and returns a step record if the step with the given name is found in the definition.
- <b>parse_definition_attribute/1</b>: A function that takes a WorkflowAttrEntity record or a tuple and returns an attribute record with the parsed properties.
- <b>parse_state_data/1</b>: A function that takes a JSON string and returns a state record with the parsed attributes and data.
- <b>format_state_data/1</b>: A function that takes a state record and returns a formatted JSON string with the state data and attributes.
- <b>find_state/2</b>: A function that takes a name and an EntityId as arguments and returns the workflow state from the database.
- <b>format/1</b>: A function that takes an im_workflow record and returns a WorkflowEntity record with the formatted properties.
- <b>format_state/1</b>: A function that takes an im_workflow_state record and returns a WorkflowStateEntity record with the formatted properties.
- <b>format_step/1</b>: A function that takes a step represented as a struct (map) and returns a WorkflowStepEntity record with the formatted properties.
- <b>format_attr/1</b>: A function that takes an attribute represented as a struct (map) or an attribute record and returns a WorkflowAttrEntity record with the formatted properties.
- <b>format_attr_value/1</b>: A function that takes a value and returns the formatted value.
- <b>json_array_or_empty/2</b>: A function that takes a field and a property list as arguments and returns a JSON array if the field is present in the property list, otherwise, returns an empty list.
- <b>ensure_sys_workflows/0</b>: A function that loads and saves the system workflows from a directory specified in the application's environment.
- <b>save/4</b>: A function that takes a name, JSON string, a boolean flag HasSuperPerms, and a boolean flag IsRunBySystem as arguments, and saves a workflow with the specified properties if it passes the necessary checks and the user has the required permissions.

This module deals with handling workflow data by parsing, formatting, and saving the necessary information. It works with different data structures, such as records and maps, and manipulates JSON strings for data storage and retrieval.

<br /><br />

<h2>im_synapse</h2>

In this module, there is only one exported function, liberate/2. This module is responsible for handling the liberation of synapses. Here's a brief breakdown of the code:

- <b>liberate/2</b>: A function that takes a LiberateSynapse record and a UserId as arguments. The purpose of this function is to broadcast a discovered synapse to the target users:
   1. First, it extracts the FeedType and FeedId from the Synapse entity.
   2. Then, it computes the target user IDs using the <b>im_message:target_user_ids/3</b> function.
   3. If the <i>SendToMe</i> flag is set to true, the current user's ID (UserId) is added to the list of <i>TargetUserIds</i>.
The function iterates through the list of <i>TargetUserIds</i> and broadcasts the discovered synapse using the <b>im_user_state:broadcast/3</b> function.
The module deals with handling synapses and broadcasting the discovered synapses to the target users. It updates and manipulates the SynapseEntity record to ensure the correct sender and feed IDs are set before broadcasting the synapse.

<br /><br />

<h2>im_migration</h2>

The <b>im_migration</b> Erlang module is responsible for handling migrations within the application. It defines a behavior with callbacks <b>up/0</b> and <b>down/0</b> and provides functions to apply, upgrade, and downgrade migrations. Here's a brief breakdown of the code:

- <b>apply/0</b>: This function applies all available migrations. It first logs the migrations list and iterates through them, calling the upgrade/1 function for each migration module.
- <b>upgrade/1</b>: This function takes a migration module as an argument and performs the following steps:
   1. It checks if the migration has already been applied using the is_applied/1 function. If so, it logs that the migration is skipped.
   2. If the migration hasn't been applied, it calls the up/0 callback function from the migration module.
   3. If the callback returns ok, it logs the successful upgrade and saves the migration record to the im_migration table.
   4. If the callback returns error, it logs the failed upgrade.
- <b>downgrade/1</b>: This function takes a migration module as an argument and performs the following steps:
   1. It checks if the migration has already been applied using the is_applied/1 function. If not, it logs that the migration is skipped.
   2. If the migration has been applied, it calls the down/0 callback function from the migration module.
   3. If the callback returns ok, it logs the successful downgrade and deletes the migration record from the im_migration table.
   4. If the callback returns error, it logs the failed downgrade.
- <b>migrations/0</b>: This function retrieves all migration modules by filtering the application's modules based on the im_migration behavior. The resulting list is sorted using lists:usort/1.
- <b>is_applied/1</b>: This function takes a migration module name as an argument and checks if the migration has been applied by querying the im_migration table. It returns true if the migration record exists and false if it doesn't.

The <b>im_migration</b> module provides a framework for managing migrations in the application, allowing for the application of new migrations and the rollback of applied migrations.

<br /><br />

<h2>im_fixtures</h2>

The <b>im_fixtures</b> Erlang module is responsible for applying fixtures to the application, such as ensuring the presence of a system user, roles, and permissions, as well as setting up required MongoDB indexes. Here's a brief breakdown of the code:

- <b>apply/0</b>: This function calls a series of ensure_* functions that apply fixtures, such as ensuring the presence of the system user, roles, permissions, and MongoDB indexes. Additionally, it asynchronously calls the drop_and_reindex_users function from the im_elastic_search module on the current node.
- <b>ensure_sys_user/0</b>: This function ensures the presence of a system user by putting a record with the SYS_USER_ID constant in the im_usr table.
- <b>ensure_roles/0</b>: This function sets up roles and permissions:
   1. It first defines a list of permissions as tuples with an identifier and a name.
   2. It then adds these permissions to the im_usr_perm table.
   3. After that, it defines a list of roles as tuples with an identifier, a name, and a list of permissions.
   4. Finally, it adds these roles to the im_usr_role table.
- <b>ensure_mongodb_indexes/0</b>: This function sets up MongoDB indexes for specific collections:
   1. It ensures a 2dsphere index on the location field of the im_feed_post collection.
   2. It ensures a 2dsphere index on the location field of the im_localized_store collection.

The im_fixtures module is used to set up the initial state of the application, including essential data like system users, roles, and permissions, as well as database indexes. This ensures that the application has the necessary data and structures in place to function properly.

<br /><br />

<h2>im_ets</h2>

The <b>im_ets</b> module provides a wrapper for Erlang's ETS (Erlang Term Storage) table operations. The module exports a set of functions that allow you to interact with ETS tables: <b>all/1</b>, <b>exists/2</b>, <b>put/2</b>, <b>modify/3</b>, <b>get_one/2</b>, and <b>delete/2</b>. These functions communicate with the ETS table through a standard worker process, <i>im_std_worker</i>, using the <b>handle/3</b> function.

Here is a brief overview of each function:

- <b>spec/0</b>: This function defines the worker specification for the ETS worker.
- <b>all/1</b>: This function retrieves all records from the specified ETS table.
- <b>exists/2</b>: This function checks if a record with the given ID exists in the specified ETS table.
- <b>put/2</b>: This function inserts a tuple into the specified ETS table.
- <b>modify/3</b>: This function modifies a record with the given ID in the specified ETS table using a provided function.
- <b>get_one/2</b>: This function retrieves a record with the given ID from the specified ETS table.
- <b>delete/2</b>: This function deletes a record with the given ID from the specified ETS table.

The <b>handle/3</b> function is responsible for handling the actual ETS operations. It pattern-matches on the provided command and delegates the operation to the corresponding helper function: <b>ensure_ets/1</b>, <b>get_one_inner/2</b>, and <b>put_inner/2</b>.

- <b>ensure_ets/1</b>: This function checks if an ETS table with the given name exists. If it doesn't, it creates a new ETS table with the specified name and options.
- <b>get_one_inner/2</b>: This function retrieves a record with the given ID from the specified ETS table, ensuring that the table exists first.
- <b>put_inner/2</b>: This function inserts a tuple into the specified ETS table, ensuring that the table exists first.

In summary, the im_ets module provides an easy-to-use interface for working with ETS tables, abstracting away the complexity of managing ETS tables and allowing you to focus on implementing your application logic.

<br /><br />

<h2>im_entities</h2>

The <b>im_entities</b> module defines the schema for the messenger backend database, which contains various tables to store information related to the application. The module exports a single function called <b>meta/0</b> that returns the metadata of the schema with the defined tables.

Here's a brief description of the tables:

- <b>im_usr</b>: Stores user data.
- <b>im_usr_role</b>: Stores user roles.
- <b>im_usr_perm</b>: Stores user permissions.
- <b>im_usr_token</b>: Stores user tokens.
- <b>im_code</b>: Stores codes for various purposes (e.g., password reset).
- <b>im_contact</b>: Stores user contact information.
- <b>im_lookup_phone</b>: Stores phone number lookups.
- <b>im_lookup_facebook</b>: Stores Facebook lookups.
- <b>im_add_contacts_task</b>: Stores tasks related to adding contacts.
- <b>im_usr_phone</b>: Stores user phone numbers.
- <b>im_feedback</b>: Stores user feedback.
- <b>im_report</b>: Stores reports generated by users.
- <b>im_msg_star</b>: Stores starred messages.
- <b>im_grp</b>: Stores group information.
- <b>im_msg</b>: Stores messages.
- <b>im_update</b>: Stores updates related to the application.
- <b>im_muc_history_marker</b>: Stores markers for multi-user chat history.
- <b>im_directory</b>: Stores directory information.
- <b>im_department</b>: Stores department information.
- <b>im_bot</b>: Stores bot information.
- <b>im_bot_server_msg</b>: Stores bot server messages.
- <b>im_bot_user_lookup</b>: Stores bot user lookups.
- <b>im_bot_username_lookup</b>: Stores bot username lookups.
- <b>im_device_locale_lookup</b>: Stores device locale lookups.
- <b>im_device_push_token_lookup</b>: Stores device push token lookups.
- <b>im_feed_post</b>: Stores feed posts.
- <b>im_feed_post_tag</b>: Stores feed post tags.
- <b>im_feed_post_category</b>: Stores feed post categories.
- <b>im_localized_store</b>: Stores localized store information.
- <b>im_csr</b>: Stores customer support request information.
- <b>im_task</b>: Stores tasks.
- <b>im_file</b>: Stores files.
- <b>im_lookup_file</b>: Stores file lookups.
- <b>im_order</b>: Stores orders.
- <b>im_order_serial_lookup</b>: Stores order serial lookups.
- <b>im_order_avail_date_reserve</b>: Stores order available date reserves.
- <b>im_call</b>: Stores call information.
- <b>im_call_user_lookup</b>: Stores call user lookups.
- <b>im_like</b>: Stores likes.
- <b>im_like_count</b>: Stores like counts.
- <b>im_page</b>: Stores page information.
- <b>im_setting</b>: Stores settings.
- <b>im_migration</b>: Stores migration information.
- <b>im_ogdata</b>: Stores Open Graph data.
- <b>im_workflow</b>: Stores workflows.
- <b>im_workflow_state</b>: Stores workflow states.
- <b>im_workflow_version</b>: Stores workflow versions.

Additionally, the schema contains several tables for the channel entity:

- <b>channel</b>: Stores channel information.
- <b>channel_post_link</b>: Stores channel post links.
- <b>channel_post</b>: Stores channel posts.
- <b>channel_location</b>: Stores channel locations.
- <b>channel_category</b>: Stores channel categories.
- <b>channel_post_share_lookup</b>: Stores channel post share lookups.
- <b>channel_post_like_lookup</b>: Stores channel post like lookups.
- <b>channel_post_thread</b>: Stores channel post threads.
- <b>channel_user_sub_lookup</b>: Stores channel user subscription lookups.
- <b>user_channel_sub_lookup</b>: Stores user channel subscription lookups.
- <b>user_channel_lookup</b>: Stores user channel lookups.

This module defines a comprehensive schema that represents various entities and their relationships within the application. It includes tables related to users, messages, groups, bots, files, orders, calls, likes, pages, settings, workflows, and channels, among others. These tables allow the application to manage and store data efficiently, ensuring that the required information is available when needed.

When working with this module or the defined schema, you would typically interact with these tables using CRUD operations (Create, Read, Update, and Delete) to insert new records, fetch existing records, update records, or remove records. The actual implementation of these operations would depend on the underlying database system and the ORM (Object-Relational Mapping) library used in the application.

To summarize, the <b>im_entities module</b> provides the structure for organizing and managing the data required by the application. This structure enables efficient data storage and retrieval, which is crucial for the smooth operation of any application.

