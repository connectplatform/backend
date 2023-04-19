<h1>Roster (/backend/apps/im/src/roster/)</h1>

The <b>/backend/apps/im/src/roster/</b> folder contains modules related to the management of user rosters in the Connect Platform. Rosters are used to store and manage user contact lists, chat room memberships, and presence information.

<br /><br />

<h2>im_roster.erl</h2>
This module provides the core functionality for managing user rosters. The main functions exported by this module include:

- <b>start_link/1</b>: Starts the gen_server for a specific user.
- <b>init/1</b>: Initializes the gen_server state.
- <b>handle_call/3</b>, handle_cast/2, handle_info/2, terminate/2, and code_change/3: gen_server behavior callbacks for handling client requests and maintaining the state.
- <b>add_contact/3</b>: Adds a new contact to the user's roster.
- <b>remove_contact/2</b>: Removes a contact from the user's roster.
- <b>update_contact/3</b>: Updates the contact's information in the user's roster.
- <b>get_contacts/1</b>: Retrieves the user's roster contacts.
- <b>get_contact/2</b>: Retrieves a specific contact from the user's roster.

<br><br>

<h2>im_roster_chat.erl</h2>

This module provides functions for managing one-to-one chat rosters. The main functions exported by this module include:

- <b>add_contact/3</b>: Adds a new contact to the user's chat roster.
- <b>remove_contact/2</b>: Removes a contact from the user's chat roster.
- <b>update_contact/3</b>: Updates the contact's information in the user's chat roster.
- <b>get_contacts/1</b>: Retrieves the user's chat roster contacts.
- <b>get_contact/2</b>: Retrieves a specific contact from the user's chat roster.

<br><br>

<h2>im_roster_muc.erl</h2>

This module provides functions for managing multi-user chat (MUC) room rosters. The main functions exported by this module include:

- <b>join_room/3</b>: Adds a user to a MUC room's roster.
- <b>leave_room/2</b>: Removes a user from a MUC room's roster.
- <b>get_rooms/1</b>: Retrieves the user's MUC room roster.
- <b>get_room/2</b>: Retrieves a specific MUC room from the user's roster.

<br><br>

<h2>im_roster_sup.erl</h2>

This module is a supervisor for the <b>im_roster</b> module. It provides a simple_one_for_one supervisor that starts a new <b>im_roster</b> worker for each user. The main functions exported by this module include:

- <b>start_link/0</b>: Starts the supervisor.
- <b>init/1</b>: Initializes the supervisor's children and strategy.

In summary, the <b>/backend/apps/im/src/roster/</b> folder contains modules responsible for managing user rosters in the Connect Platform, including contact lists, one-to-one chat rosters, and multi-user chat room rosters. These modules provide functions for adding, removing, updating, and retrieving roster contacts, as well as managing MUC room memberships. The <b>im_roster_sup</b> module supervises the roster-related worker processes.