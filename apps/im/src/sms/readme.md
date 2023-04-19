<h1>SMS Module</h1>

These files handle SMS sending. The modules and their functionalities are as follows:

- <b>im_sms.erl</b>: This module is the main entry point for sending SMS messages. It exports the send/3 function that takes a country code, phone number, and text. The module selects a primary and an alternative SMS provider based on the country code, then attempts to send the SMS using the primary provider. If the primary provider fails, it tries using the alternative provider.
- <b>im_sms_sup.erl</b>: This module is the supervisor for the SMS sending application. It defines the start_link/0 and init/1 functions to create a supervision tree. It supervises the following child processes: im_sms_twilio, im_sms_alpha, im_sms_thailand, and im_sms_nexmo. These child processes correspond to the different SMS providers.

The <b>im_sms</b> module works together with the provider modules and the supervisor module to provide SMS sending functionality for the application. Depending on the country code, the application will use the appropriate provider to send the SMS. If the primary provider fails, the application will attempt to use the alternative provider.

There are additional modules for each SMS provider (Twilio, AlphaSMS, Nexmo, and Thailand). 
Each provider has two modules: a main module and a worker module. The main modules are <b>im_sms_twilio.erl</b>, <b>im_sms_alpha.erl</b>, <b>im_sms_nexmo.erl</b>, and <b>im_sms_thailand.erl</b>. These modules handle the provider-specific implementation of sending SMS messages. The worker modules, <b>im_sms_twilio_worker.erl</b>, <b>im_sms_alpha_worker.erl</b>, <b>im_sms_nexmo_worker.erl</b>, and <b>im_sms_thailand_worker.erl</b>, contain the workers for each provider.

All worker processes are supervised using the <i>simple_one_for_one</i> supervision strategy, which means a new worker process is started for each request and terminated after processing the request. 
This allows for concurrent handling of multiple SMS sending requests.

<br /><br />

<h1>AlphaSMS</h1>

The two files <b>im_sms_alpha.erl</b> and <b>im_sms_alpha_worker.erl</b> implement the SMS sending functionality using the AlphaSMS provider. Here's an overview of each module:

- <b>im_sms_alpha.erl</b>: This module is the supervisor for the AlphaSMS worker processes. It exports the start_link/0, init/1, and send/3 functions. The send/3 function is the entry point for sending SMS messages using AlphaSMS. It starts a new worker process, sends a request to the worker, and then terminates the worker after the request is processed.
- <b>im_sms_alpha_worker.erl</b>: This module is a gen_server implementation that handles the actual SMS sending using the AlphaSMS provider. It exports functions such as init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, and code_change/3. The primary function of interest is handle_call/3, which processes the request to send an SMS. It constructs an XML body containing the necessary information, sends an HTTP request to the AlphaSMS API, and processes the response. The check_sms/2 function is used to check the delivery status of the SMS, and it retries the check until the message is delivered or an error occurs.

The <b>im_sms_alpha</b> and <b>im_sms_alpha_worker</b> modules work together to provide SMS sending functionality using the AlphaSMS provider. When a request to send an SMS is made, the <b>im_sms_alpha</b> supervisor creates a new worker process and sends the request to the worker. The worker then communicates with the AlphaSMS API to send the SMS and check its delivery status. Once the request is complete, the worker is terminated by the supervisor.


<br /><br />


<h1>Twilio SMS Gateway</h1>

The Twilio SMS gateway implementation in Erlang is divided into two parts. The first part is the supervisor, which manages worker processes that handle sending SMS messages. The second part is the worker module, which is responsible for making the actual HTTP requests to the Twilio SMS gateway API:

- <b>im_sms_twilio.erl</b>: This supervisor exports start_link/0, init/1, and send/3 functions. The start_link/0 function starts the supervisor and registers it with a name. The init/1 function initializes the supervisor with environment variables containing necessary API keys and other configuration values. The send/3 function sends an SMS message by starting a worker process and making a call to it with the necessary parameters.
- <b>im_sms_twilio_worker.erl</b>: This worker module exports functions like <b>start_link/4</b>, <b>init/1</b>, <b>handle_call/3</b>, <b>handle_cast/2</b>, <b>handle_info/2</b>, <b>terminate/2</b>, and <b>code_change/3</b>. The <b>start_link/4</b> function starts the worker process, and the <b>init/1</b> function initializes its state with the necessary API keys and configuration values. The <b>handle_call/3</b> function is responsible for making the actual HTTP request to the Twilio SMS gateway API and handling the response.


<br /><br />


<h1>Nexmo SMS Gateway</h1>

The Nexmo SMS gateway implementation in Erlang is divided into two parts. The first part is the supervisor, which manages worker processes that handle sending SMS messages. The second part is the worker module, which is responsible for making the actual HTTP requests to the Nexmo SMS gateway API:

- <b>im_sms_nexmo.erl</b>: This supervisor exports start_link/0, init/1, and send/3 functions. The start_link/0 function starts the supervisor and registers it with a name. The init/1 function initializes the supervisor with environment variables containing necessary API keys and other configuration values. The send/3 function sends an SMS message by starting a worker process and making a call to it with the necessary parameters.
- <b>im_sms_nexmo_worker.erl</b>: This worker module exports functions like <b>start_link/4</b>, <b>init/1</b>, <b>handle_call/3</b>, <b>handle_cast/2</b>, <b>handle_info/2</b>, <b>terminate/2</b>, and <b>code_change/3</b>. The <b>start_link/4</b> function starts the worker process, and the <b>init/1</b> function initializes its state with the necessary API keys and configuration values. The <b>handle_call/3</b> function is responsible for making the actual HTTP request to the Nexmo SMS gateway API and handling the response.

The HTTP request is made using the <b>httpc:request/3</b> function, and the response is processed to handle various HTTP status codes and errors. 

<br /><br />

<h1>Thailand</h1>

This is a custom SMS gateway implementation for sending SMS in Thailand. The code has two modules: <b>im_sms_thailand</b> and <b>im_sms_thailand_worker</b>.

The <b>im_sms_thailand</b> module is a supervisor that manages worker processes. It exports <b>start_link/0</b>, <b>init/1</b>, and <b>send/3</b> functions. The <b>start_link/0</b> function starts the supervisor, which initializes the worker processes by reading the group code and token from the application environment. The supervisor uses a <i>simple_one_for_one</i> strategy, allowing multiple workers to be started as needed.

The <b>send/3</b> function in the <b><i>im_sms_thailand</i></b> module is used to send SMS messages. It starts a new worker, sends the message using the worker, and terminates the worker afterward. It also handles timeouts and returns an error in case of a timeout.

The <b>im_sms_thailand_worker</b> module is a <i>gen_server</i> that handles sending SMS messages to the Thailand SMS gateway. It exports <b>init/1</b>, <b>handle_call/3</b>, <b>handle_cast/2</b>, <b>handle_info/2</b>, <b>terminate/2</b>, and <b>code_change/3</b> functions. The <b>start_link/2</b> function starts a new <i>gen_server</i> with the given group code and token.

The <b>init/1</b> function initializes the state of the gen_server with the provided group code and token. The <b>handle_call/3</b> function handles the send call, which sends the SMS message to the Thailand SMS gateway. It constructs the request body with the group code, SMS text, recipients, and asynchronous flag. The request is then sent to the SMS gateway using the <b>httpc:request/4</b> function. The response is parsed, and an appropriate result (either ok or an error) is returned.

The other functions in the <b><i>im_sms_thailand_worker</i></b> module are standard <i>gen_server</i> callbacks that handle various server lifecycle events.
