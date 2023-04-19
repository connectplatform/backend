<h1>Utils (/backend/apps/im/src/utils/)</h1>

The <i>/backend/apps/im/src/utils/</i> folder contains utility modules that provide various helper functions and services for the application. These utilities are not directly related to any specific functionality but are used throughout the application to support various operations.

 - <b>im_cron.erl</b> - This module provides a simple scheduler for running periodic tasks. It exports functions like start_link/0, init/1, handle_info/2, code_change/3, and terminate/2. This module is used to periodically execute certain tasks within the application, such as cleanup or data updates.

- <b>im_encode_uri.erl</b> - This module provides utility functions for encoding and decoding URIs, including encode/1, decode/1, and decode_unsafe/1. These functions are used throughout the application to ensure proper handling of URIs and URL components.

- <b>im_geo_utils.erl</b> - This module provides utility functions related to geographic calculations, including distance/4 and get_bounding_box/3. These functions are used to calculate distances and bounding boxes for various geolocation-based features in the application.

- <b>im_gmaps.erl</b> - This module provides a wrapper for the Google Maps API, allowing the application to interact with Google Maps services. Functions like geocode/1, reverse_geocode/1, timezone/1, and directions/3 are exported. These functions are used to perform various geolocation-based operations, such as retrieving address information and calculating directions.

- <b>im_http.erl</b> - This module provides a simple HTTP client for making requests to external APIs or services. It exports functions like get/1, post/2, put/2, delete/1, request/5, and request/6. This module is used to interact with external services or APIs from within the application.

- <b>im_og.erl</b> - This module provides functions for parsing OpenGraph tags from HTML content. It exports functions like parse/1 and parse/2. These functions are used to extract metadata from web pages, such as title, description, and image URLs, which can be used for generating link previews or other content-related features in the application.

In summary, the /backend/apps/im/src/utils/ folder contains utility modules that support various operations throughout the Connect Platform backend application. These utilities provide functionality related to scheduling tasks, URI encoding, geographic calculations, interaction with Google Maps API, simple HTTP client, and parsing OpenGraph tags. 

Detailed documentation for each utility is below:




<h2>im_cron.erl</h2>

The im_cron module is responsible for managing scheduled tasks (cron jobs) in the application. It includes three exported functions: init/0, statistics_of_the_week_for_vendor_job/0, and change_cron_time/1.

- <b>init/0</b>: Initializes and schedules the cron jobs. If the application is running in debug mode, it schedules the archive_expired_tickets_job() to run every second. In non-debug mode, it schedules the archive_expired_tickets_job() to run every 30 minutes and the statistics_of_the_week_for_vendor_job() to run weekly on Mondays at 2 AM.
- <b>statistics_of_the_week_for_vendor_job/0</b>: This function calculates the statistics for vendors by fetching their grouped statistics using the im_statistics:get_grouped_vendor_statistics/4 function. It then notifies the vendors with the calculated statistics using the im_order_report:notify_statistics_of_the_week_for_vendor/2 function.
- <b>archive_expired_tickets_job/0</b>: This function archives expired tickets. It checks if the application is running in debug mode, and if so, it sets the expiration period to the current timestamp. Otherwise, it sets the expiration period to 12 hours ago. The function then updates the status of the expired tickets to ?ORDER_STATUS_ARCHIEVED.
- <b>change_cron_time/1</b>: Takes a 'ChangeCronTime' record as input. If the application is running in debug mode, it changes the internal datetime of the erlcron library to the provided timestamp. It returns a 'ChangeCronTimeResp' record with the new timestamp. If the application is running in non-debug mode, it returns an error response with an invalid message error code.

The main purpose of this module is to manage scheduled tasks within the application, such as archiving expired tickets and sending weekly statistics to vendors.




<h2>im_encode_uri.erl</h2>

The im_encode_uri module provides functionality for URL encoding. It has a single exported function, encode/1, which takes a list of characters (a string) as input and returns the URL-encoded version of the string.

The encode/1 function processes each character in the input string and applies the appropriate URL encoding rules. It handles various character types, such as:

Lowercase letters (a-z), uppercase letters (A-Z), and numbers (0-9) are left unchanged.
The space character is converted to '+'.
Unreserved characters, such as '-', '_', '.', '!', '~', '*', ''', '(', and ')' are left unchanged.
Characters in the range of 0x7F to 0x07FF are encoded using a two-byte escape sequence.
Characters greater than 0x07FF are encoded using a three-byte escape sequence.
The hex_octet/1 function converts a given integer into its hexadecimal representation, and the escape_byte/1 function URL-encodes a single character using the % symbol followed by the hexadecimal value of the character.

The normalize/1 function ensures that hexadecimal values with a single character are prepended with a '0' so that the result is always two characters long.

This module is useful when you need to URL-encode strings in an Erlang application, particularly when dealing with HTTP requests or other scenarios where URL encoding is required.




<h2>im_geo_utils.erl</h2>

The im_geo_utils module provides utility functions for working with geographical coordinates and distances. It exports the following functions:

- <b>calc_distance/2</b>: Calculates the distance (in meters) between two geographical coordinates, given as tuples of latitude and longitude. It uses the Haversine formula to determine the great-circle distance between the two points on the Earth's surface. The function returns the calculated distance as a rounded integer value.
- <b>filter_coor/1</b>: Filters a coordinate value, ensuring that it is a valid number. If the input is already a number, it returns the input unchanged. If the input is not a number, it attempts to convert it to a float value. If the conversion is unsuccessful, it returns the default value (0.0).
- <b>filter_distance/1</b>: Filters a distance value, ensuring that it is a valid number and within the allowed range (1 to 1,000,000,000). If the input is already a number, it checks if it is within the allowed range. If the input is not a number, it attempts to convert it to a float value and checks if it is within the allowed range. If the conversion is unsuccessful, it returns the default value (200).
- <b>filter_limit/1</b>: Filters a limit value, ensuring that it is a valid integer and within the allowed range (1 to 1,000,000,000). If the input is already a number, it checks if it is within the allowed range. If the input is not a number, it attempts to convert it to a float value, rounds it to the nearest integer, and checks if it is within the allowed range. If the conversion is unsuccessful, it returns the default value (50).
- <b>check_range/3</b>: Checks if a given value is within a specified range. If the value is outside the range, it returns the corresponding boundary value (either the minimum or maximum value). If the value is within the range, it returns the value unchanged.
- <b>format_float/2</b>: A helper function that attempts to convert a binary or list value to a float. If the conversion is unsuccessful, it returns the provided default value.

The im_geo_utils module can be useful in applications that deal with geographic data, such as calculating distances between locations or filtering and validating input data related to geographic coordinates, distances, and limits.




<h2>im_gmaps.erl</h2>

The im_gmaps module provides functionality for interacting with the Google Maps API, including suggesting locations, fetching location details by GPS coordinates, and submitting locations. It also calculates the radius of a location and handles various utility functions for interacting with the Google Maps API.

The main exported functions are:

- <b>suggest_location/2</b>: Takes a SuggestLocation record and a user ID. Returns a SuggestLocationResp record containing a list of suggested locations based on the input text.
- <b>location_by_gps/2</b>: Takes a LocationByGps record and a user ID. Returns a LocationByGpsResp record containing the location information based on the provided GPS coordinates.
- <b>submit_location/1</b>: Takes a location ID and returns the submitted location's details after interacting with the Google Maps API.

There are also utility functions for fetching JSON data from the API, calculating the radius and half-distance of a location, and converting coordinates to binary strings.

In addition, it defines constants for Earth radius, fallback locale, and the Google Maps API key.




<h2>im_http.erl</h2>

The im_http module provides a set of utility functions to perform HTTP requests. It uses the Hackney library to make these requests and exports several functions to allow for various types of HTTP requests: GET, POST, PUT, DELETE, and HEAD.

Here is a breakdown of the functions in the module:

- <b>get/1, get/2, get/3</b>: Functions for performing HTTP GET requests with varying numbers of arguments.
- <b>post/2, post/3, post/4</b>: Functions for performing HTTP POST requests with varying numbers of arguments.
- <b>put/2, put/3, put/4</b>: Functions for performing HTTP PUT requests with varying numbers of arguments.
- <b>delete/1, delete/2, delete/3</b>: Functions for performing HTTP DELETE requests with varying numbers of arguments.
- <b>head/1, head/2, head/3</b>: Functions for performing HTTP HEAD requests with varying numbers of arguments.
- <b>request/2, request/3, request/4, request/5</b>: Generic request functions to perform HTTP requests with varying numbers of arguments.

The functions with varying numbers of arguments allow users to provide different sets of options, headers, and other configurations when making HTTP requests.

The request/6 function is a private function that handles the actual HTTP request using Hackney. It takes the following arguments:

- <b>Method</b>: The HTTP method (e.g., get, post, put, delete, head).
- <b>Url</b>: The URL to send the request to.
- <b>Body</b>: The request body, if any.
- <b>Headers</b>: The request headers, if any.
- <b>Options</b>: Additional options for the Hackney request.
- <b>Retries</b>: The number of times the request has been retried.

The function first tries to make an HTTP request using the Hackney library. If the request is successful, it processes the response and returns a tuple with the response status, body, and headers. If the request fails, it retries up to 3 times before returning an error.




<h2>im_og.erl</h2>

The <i>im_og</i> module provides utility functions to fetch Open Graph (OG) data and locate users by IP addresses. It exports the following functions:

- <b>locate_by_ip/2</b>: Locates a user by their IP address, returning information about their location, such as country, region, city, and latitude/longitude coordinates.
- <b>fetch/2</b>: Fetches Open Graph (OG) data for a given URL, returning information such as title, description, image, domain, and other metadata.
- <b>fetch_url/4</b>: A private function that fetches OG data, either from cache or by making an HTTP request to the specified URL. The function takes the URL, a cache-only flag, a force refetch flag, and the user ID as arguments.
- <b>refetch/2 and refetch/3</b>: Private functions that refetch OG data for a given URL, either by simulating a fetch in debug mode or by making an HTTP request to the specified URL.

The module also includes some helper functions for formatting media lists, formatting media, and fetching favicon paths:

- <b>locate_by_ip/2</b> - This function sends an HTTP GET request to a location service using the im_http:get/3 function, specifying the IP address as a query parameter. It then processes the response and returns a LocateByIpResp7 record containing location data.
- <b>fetch/2</b> - This function first checks if the OG data should be refetched, and if so, calls the refetch/3 function. Otherwise, it attempts to fetch the OG data from cache using the ctail:get/2 function. If the data is not in cache, the fetch_url/4 function is called to fetch the data.
- <b>refetch/3</b> - This function simulates a fetch in debug mode by returning hard-coded example data. In non-debug mode, it sends an HTTP GET request to the specified URL using the im_http:get/3 function and then processes the response. If the response contains valid OG data, the function updates the cache with the new data and returns the fetched data.
- <b>get_favicon_path/1</b> - This is a helper function that attempts to find a favicon for a given URL by first checking if a .ico file is present and then checking for a .png file. If neither file is found, it returns <i>undefined</i>.