<h2>hydratorNew.js</h2>

This hydratorNew.js file defines a module called Hydrator that can encode and decode data using a predefined map. The main purpose of this module is to create binary streams from UTF data. Here's a breakdown of its functionality:

The module has four public functions: encode, decode, generateEntity, and fetchMap.
- <b>fetchMap(cb)</b> fetches the map from a JSON file located at /static/data/map.json using an XMLHttpRequest. This map is used to convert data between different formats.
- <b>decode(response)</b> takes a response object, checks if it has the proper format, and then calls the hydrate function to decode the data.
- <b>generateEntity(modelName, data = {})</b> generates an entity object with the specified model name and data. It first initializes an empty object with default values and then populates it with the provided data. It also handles nested objects and different data types.
- <b>getEmpty(model)</b> returns an empty object with default values for the specified model.
- <b>hydrate functions (e.g., hydrateBoolean, hydrateInteger, hydrateReal, hydrateString)</b> are used to convert and validate data for different data types.
- <b>hydrate(message)</b> processes the message and populates the result object with the appropriate data using the hydrate functions mentioned above. It also handles different field types, such as sets, sequences, and more.
- <b>encode(sequenceName, obj)</b> encodes an object into a binary stream. It creates an Erlang tuple with the given object's data and the corresponding field types from the map.

Utility functions, such as objectSize, utf8ToByteArray, and utf8ArrayToStr, help with encoding and decoding tasks, such as converting between string and byte array representations.
In summary, this Hydrator module is responsible for encoding and decoding data into binary streams using a predefined map. It has functions for handling various data types and field types, as well as utility functions to assist with the conversion tasks.

