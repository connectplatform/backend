<h1>ID Reports</h1>

<h2>im_feedback</h2>

This module, im_feedback, deals with adding user feedback to the system. It exports one function, <b>add/2</b>, which takes two arguments: a record 'AddFeedback' and a user record or undefined.

The <b>add/2</b> function performs a series of validations on the input parameters before storing the feedback using the <b>ctail:put/1</b> function. The validations include:
 1. Ensuring the name and email fields are not undefined.
 2. Checking that the topic and message fields are not empty and their length doesn't exceed the defined limits (?TOPIC_SIZE and ?BODY_SIZE).
 3. Verifying that the client field is not 0 (undefined).
 4. If any of these validations fail, an error response is returned. Otherwise, the feedback is stored using <b>ctail:put/1</b>, and an <i>#'AddFeedbackResp'{}</i> record is returned to indicate success.

The main purpose of this module is to provide a way to add user feedback to the system, ensuring that the input is valid before saving it.

<br /><br />

<h2>im_report</h2>

The <b>im_report</b> module is responsible for handling user reports and sending report emails. It includes three exported functions: <b>add/2</b>, <b>add_with_locale/2</b>, and <b>send_report_email/4</b>.

- <b>add/2</b>: Takes an 'AddReport' record and an im_usr record as input. It calls the <b>add_with_locale/2</b> function using the <i>'AddReport'</i> record and the user record, with the default locale.
- <b>add_with_locale/2</b>: Takes an 'AddReportWithLocale' record and an im_usr record as input. This function performs input validation for the name, email, topic, and information fields, and checks if the length of the information and topic fields is within the allowed limits. If the input is valid, it creates an im_report record and stores it using the <b>ctail:put/1</b> function. It then calls the <b>send_report_email/4</b> function to send an email.
- <b>send_report_email/4</b>: Takes an email address, message topic, message kind, and locale as input. This function sends a report email to the specified email address. It fetches the SMTP relay, username, and password from the environment, reads the email template file based on the provided locale, and replaces placeholders in the email template with the message topic and report type. Finally, it sends the email using the <b>gen_smtp_client:send/2</b> function with the appropriate options.

The primary purpose of this module is to handle user reports, store them, and send report emails to users.

