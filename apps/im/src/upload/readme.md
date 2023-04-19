<h1>Upload</h1>

These modules work together to provide file upload functionality for the application. Depending on the application configuration, files can be uploaded and stored either locally or in an Amazon S3 bucket. 




<h2>im_filestore_common.erl</h2>

This module contains utility functions shared by other modules for handling file storage. It includes functions to prepare filenames, generate random strings and determine the folder based on the file type:

- <b>prepare_filename/1</b>: Prepares a filename by removing any invalid characters and replacing them with an underscore.
- <b>random_string/1</b>: Generates a random string of the specified length.
- <b>get_folder_by_type/1</b>: Determines the folder in which to store the file based on its type (e.g., image, audio, video, or document).




<h2> im_filestore_local.erl</h2>

This module deals with the local file storage implementation. It is responsible for uploading media files to the local file system and storing relevant information in a database:

- <b>init/1</b>: Initializes the local file storage with the necessary configuration.
- <b>store/3</b>: Uploads a file to the local file system and stores its metadata (filename, size, type, and folder) in the database.
- <b>delete/1</b>: Deletes a file from the local file system and removes its metadata from the database.
- <b>get_file_by_id/1</b>: Retrieves a file's metadata from the database using its ID.



<h2>im_filestore_s3.erl</h2>

This module deals with the Amazon S3 storage implementation. It is responsible for uploading media files to an Amazon S3 bucket, returning the remote URL for the uploaded file:

- <b>init/1</b>: Initializes the Amazon S3 storage with the necessary configuration.
- <b>store/3</b>: Uploads a file to an Amazon S3 bucket and returns the remote URL.
- <b>delete/1</b>: Deletes a file from the Amazon S3 bucket.
- <b>get_file_by_id/1</b>: Not implemented in this module, as the functionality is provided by the im_filestore_common.erl module.




<h2>im_uploader.erl</h2>

This module is the main entry point for handling file uploads. It routes the request to either local file storage or Amazon S3 storage based on the application configuration.

- <b>start_link/0</b>: Starts the uploader process.
- <b>init/1</b>: Initializes the uploader with the necessary configuration.
- <b>handle_call/3</b>: Handles synchronous calls made to the uploader process.
- <b>handle_cast/2</b>: Handles asynchronous calls made to the uploader process.
- <b>handle_info/2</b>: Handles system messages and other information sent to the uploader process.
- <b>terminate/2</b>: Cleans up resources when the uploader process is terminated.
- <b>code_change/3</b>: Handles changes in the code during a hot code upgrade.

This module routes the request to either local file storage or Amazon S3 storage based on the application configuration. The routing is handled in the handle_call/3 function, which checks the storage_type configuration and calls either im_filestore_local:store/3 or im_filestore_s3:store/3 accordingly.