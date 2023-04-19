<h1>CMS Module</h1>

These files work together to provide a comprehensive product and order management system, access control, data formatting, utility functions, user state management, transaction execution, and order reporting functionality for the application.

<br><br>

<h2>im_product.erl</h2>

This file deals with product management, including creating, updating, and deleting products. The main product-related functions are:

- <b>create_product/2</b>: Creates a new product and adds it to the database.
- <b>update_product/2</b>: Updates an existing product with new information.
- <b>delete_product/2</b>: Deletes a product from the database.
- <b>get_product/2</b>: Retrieves a product from the database based on the product ID.
- <b>get_products/2</b>: Retrieves a list of products from the database based on specified filter criteria.

These functions are related to managing orders, including placing, changing, charging, using, and deleting orders:

- <b>get_order/2</b>: Retrieves an order based on its ID or serial.
- <b>change_order/2</b>: Modifies an order, updating its quantity and date.
- <b>charge_order/2</b>: Processes the payment for an order.
- <b>validate_and_calc_amount/1</b>: Validates order information and calculates the total amount for a set of orders.
- <b>use_order/2</b>: Marks an order as used and updates the available quantity.
- <b>delete_order/2</b>: Deletes an order from the database.

<br><br>

<h2>im_acl.erl</h2>

This file deals with access control and user permissions management. The main functions are:

- <b>has_role/2</b>: Checks if a user has a specific role.
- <b>add_role/2</b>: Adds a role to a user.
- <b>remove_role/2</b>: Removes a role from a user.

<br><br>

<h2>im_dto.erl</h2>

This file is responsible for converting internal data structures into a format that can be easily consumed by external clients. The main functions are:

- <b>format_product/1</b>: Formats a product data structure for external consumption.
- <b>format_order/1</b>: Formats an order data structure for external consumption.

<br><br>

<h2>im_common.erl</h2>

This file contains utility functions that are commonly used across the application. The main functions are:

- <b>parse_id/1</b>: Parses an ID from a formatted string.
- <b>format_id/1</b>: Formats an ID as a string.
- <b>ensure_list/1</b>: Ensures the input is in the form of a list.
- <b>crypto_random_string/0</b>: Generates a random string.

<br><br>

<h2>im_user_state.erl</h2> 

This file manages user state and broadcasting changes to clients. The main functions are:

- <b>broadcast/4</b>: Sends a message to a list of user IDs with optional state updates.

<br><br>

<h2>im_transaction.erl</h2>

This file deals with transaction execution and management. The main functions are:

- <b>execute_all/3</b>: Executes a series of transactions in a controlled environment.

<br><br>

<h2>im_order_report.erl</h2>

This file is responsible for generating reports related to orders. The main functions are:

- <b>notify_order_purchased/2</b>: Sends a notification when an order is purchased.


<h1>Detailed Description of Functions</h2>

<h2>im_product.erl</h2>
The <b>im_product.erl</b> file is responsible for product and order management in the application. It includes functions for creating, updating, and deleting products, as well as managing orders, such as placing, changing, charging, using, and deleting orders.

<h3>Product Management Functions</h3>
<h4>create_product/2</h4>
- <b>Purpose</b>: Creates a new product and adds it to the database.
- <b>Arguments</b>:
    - A Product message record with the product information.
    - The user ID of the user creating the product.
- <b>Returns</b>: A ProductResp message record with the created product's information.

<h4>update_product/2</h4>
- <b>Purpose</b>: Updates an existing product with new information.
- <b>Arguments</b>:
    - A Product message record with the updated product information.
    - The user ID of the user updating the product.
- <b>Returns</b>: A ProductResp message record with the updated product's information.

<h4>delete_product/2</h4>
- <b>Purpose</b>: Deletes a product from the database.
- <b>Arguments</b>:
    - A DeleteProduct message record with the product ID to be deleted.
    - The user ID of the user deleting the product.
- <b>Returns</b>: A DeleteProductResp message record confirming the deletion.

<h4>get_product/2</h4>
- <b>Purpose</b>: Retrieves a product from the database based on the product ID.
- <b>Arguments</b>:
    - A GetProduct message record with the product ID to be retrieved.
    - The user ID of the user requesting the product.
- <b>Returns</b>: A ProductResp message record with the retrieved product's information.

<h4>get_products/2</h4>
- <b>Purpose</b>: Retrieves a list of products from the database based on specified filter criteria.
- <b>Arguments</b>:
    - A GetProducts message record with the filter criteria for retrieving products.
    - The user ID of the user requesting the list of products.
- <b>Returns</b>: A ProductsResp message record with the list of retrieved products.

<br>

<h3>Order Management Functions</h3>

<h4>get_order/2</h4>
- <b>Purpose</b>: Retrieves an order based on its ID or serial.
- <b>Arguments</b>:
    - An Order message record with the order reference, order ID, and serial.
    - The user ID of the user requesting the order.
- <b>Returns</b>: An OrderResp message record with the retrieved order's information.

<h4>change_order/2</h4>
- <b>Purpose</b>: Modifies an order, updating its quantity and date.
- <b>Arguments</b>:
    - A ChangeOrder message record with the order reference and updated order information.
    - The user ID of the user changing the order.
- <b>Returns</b>: A ChangeOrderResp message record with the modified order's information.

<h4>charge_order/2</h4>
- <b>Purpose</b>: Processes the payment for an order.
- <b>Arguments</b>:
    - A ChargeOrder message record with the order reference, token, and order IDs.
    - The user ID of the user charging the order.
- <b>Returns</b>: A ChargeOrderResp message record with the charged orders' information or an ErrorResp message record if the charge fails.

<h4>validate_and_calc_amount/1</h4>
- <b>Purpose</b>: Validates order information and calculates the total amount for a set of orders.
- <b>Arguments</b>:
    - A list of order IDs.
- <b>Returns</b>: A tuple with the orders, products, and total amount if validation is successful or an error code if validation fails.

<h4>use_order/2</h4>
- <b>Purpose</b>: Marks an order as used and updates the available quantity.
- <b>Arguments</b>:
    - A UseOrder message record with the order reference, serial, and quantity to be used.
    - The user ID of the user using the order.
- <b>Returns</b>: A UseOrderResp message record with the updated order's information or an ErrorResp message record if the operation fails.

<h4>delete_order/2</h4>
- <b>Purpose</b>: Deletes an order from the database.
- <b>Arguments</b>:
    - A DeleteOrder message record with the order reference and order ID to be deleted.
    - The user ID of the user deleting the order.
- <b>Returns</b>: A DeleteOrderResp message record confirming the deletion.

<br>

<h3>Helper Functions</h3>

<h4>validate_available_date/4</h4>
- <b>Purpose</b>: Validates if the requested date is available for an order based on the product type and available dates.
- <b>Arguments</b>:
    - The product type.
    - The list of available dates.
    - The requested quantity.
    - The requested date.
- <b>Returns</b>: 'ok' if the date is available and valid, otherwise an error code.

<h4>find_order/4</h4>
- <b>Purpose</b>: Finds an existing order with the same user ID, product, status, and date.
- <b>Arguments</b>:
    - The user ID.
    - The product record.
    - The order status.
    - The order record to compare with.
- <b>Returns</b>: The existing order if found, otherwise 'undefined'.

<h4>reserve_for_available_date/3</h4>
- <b>Purpose</b>: Updates the available dates for a product by reserving or releasing the specified quantity for a particular date.
- <b>Arguments</b>:
    - The product record.
    - The date to update.
    - The quantity to reserve or release.
- <b>Returns</b>: None.

This detailed documentation covers all functions and their purposes in the <b>im_product.erl</b> file, which is responsible for managing products and orders within the application.