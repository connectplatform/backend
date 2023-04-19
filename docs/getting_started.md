# Table of Contents

   * [Getting Started](#getting-started)
      * [Sending and receiving messages](#sending-and-receiving-messages)
      * [Authentication](#authentication)
         * [Authenticate using phone number](#authenticate-using-phone-number)
         * [Authenticate through Facebook](#authenticate-through-facebook)
      * [Users and contacts](#users-and-contacts)
         * [Your profile](#your-profile)
            * [Update profile](#update-profile)
            * [Get your profile](#get-your-profile)
         * [Contacts](#contacts)
            * [Add](#add-contact)
            * [Update](#update-contact)
            * [Delete](#delete-contact)
            * [Block](#block-contact)
            * [UnBlock](#unblock-contact)
            * [AddToSpam](#add-to-spam-contact)
            * [Get](#get-contact)
            * [Synchronize](#synchronize-contacts)
            * [Event ContactChanged](#event-contactChanged)
      * [Messanging](#messanging)
         * [Chats](#chats)
         * [Updates](#updates)
         * [Send message](#send-message)
         * [Retrieve messages](#retrieve-messages)
         * [Pointers](#pointers)
            * [Delivered](#delivered)
            * [Seen](#seen)
      * [Calls](#calls)
         * [Dial](#dial)
         * [Answer](#answer)
         * [Hangup](#hangup)
      * [Bots](#bots)
         * [Bot metadata](#bot-metadata)
         * [Reply keyboard](#reply-keyboard)
         * [Inline keyboard](#inline-keyboard)
         * [Inline queries](#inline-queries)
         * [Commands](#commands)

# Getting Started

## Sending and receiving messages

All messages you can send or receive from backend are described in our [ASN.1 specification](https://git.connect.software/platform/protocol/blob/master/priv/IM.asn1). In this spec you will find: enums, requests, responses, server-side push messages. Some part of these messages is explained in this tutorial.

You can send messages to backend through HTTPS or WSS:
* `https://{app}.connect.software/api/v1` (POST binary as body)
* `wss://{app}.connect.software/ws` (just send binary)

In this example `{app}` is name of the app, ex.: prod, dev-api, dev-pt1, pt1, holideum.

Backend uses BERT binary format to encode/decode messages. Thats why you must use `hydrator`. Example from JS API:

```js
const binaryEncodedMsg = Erl.encode(Hydrator.encode(name, data));
const decodedMsg = Hydrator.decode(Erl.decode(binaryEncodedMsg));
```

In the example above `Erl` is the library that encodes/decodes BERT to intermediate JavaScript representation and `Hydrator` is our class that converts it to human readable form.

We also wrote hydrators for iOS and Android. Working with `Hydrator` in iOS:

```swift
Hydrator.encodeAsync(msg) {(body, err) -> (Void) in /* code */ }
Hydrator.decodeAsync(data!, callback: {(responseMsg, err) -> (Void) in /* code */ }
```

and in Android:

```java
RosterObject rosterObject = RosterObjectHydrator.generateClassFromByteArray(bb.getAllByteArray());
final byte[] tuple = RosterObjectHydrator.generateBytesToSendFromObject(object);
```

Almost every message in protocol has `ref` field. When you send message to backend you generate some random number and put it in `ref`. Later when the message with the same `ref` value comes from HTTPS or WSS connection you can call corresponding callback.

## Authentication

Either you use HTTPS or WS you mast pass your authentication token as query string parameter, ex.: `https://{app}.connect.software/api/v1?token={token}`.

To get token you must authenticate using phone number SMS confirmation or social network login.

### Authenticate using phone number

1) Request SMS with confirmation code:

```js
send('RequestVerification', {
  phone: "+380931234567",
  deviceType: enum.DeviceType.mobile
}, response => {
  if (response.modelName === 'ErrorResp') {
    throw new Error('[' + response.code + '] ' + response.message);
  }
  // backend allows us to send SMS to the same number once in two minutes
  this.canSendSMSAgainInTime = this.timeService.getTimestamp() + TWO_MINUTES;
});
```

**NOTICE**: We use JS syntax for examples through this manual. Here we call function `send` to send message `RequestVerification` to backend, second argument is message data and the last argument is callback that we use to process response from server.

**NOTICE**: Response can be `ErrorResp` and its not what we expected to get. In the example above we expected to get `RequestVerificationResp`. You should always keep that it mind. List of possible error codes can be found in [ASN.1 specification](https://git.connect.software/platform/protocol/blob/master/priv/IM.asn1#L6).

**WARNING**: We don't do error handling and other useful checks in our examples, you must do it on your own!

2) Confirm code from SMS:

```js
send('ConfirmVerification', {
  phone: "+380931234567",
  smsCode: "123456",
  deviceId: "fed649b7-058c-464c-83bb-f1393eb6d0a1",
  deviceName: "iPhone 6S"
}, response => {
  this.setToken(response.token);
});
```

Now you can send messages to backend as authenticated user.

**NOTICE**: For WS connections, backend requires that you wait for `AuthResp` message before you send any message to backend. If your token is invalid, after timeout backend will close your WS connection.

### Authenticate through Facebook

To start facebook auth process you must open facebook gateway url, ex.: `https://{app}.connect.software/app/facebook/login?deviceId=fed649b7-058c-464c-83bb-f1393eb6d0a1&deviceName=iPhone+6S`. You can use WebView or iframe to embed facebook gateway.

After successful authentication page will redirect to: `https://{app}.connect.software/app/facebook/redirect?token={token}`. 

You must catch this redirect and extract token. Thats all, now you can send messages to backend.

## Users and contacts

### Your profile

#### Update profile

After first successful authentication new user will be created on backend. But if you authenticated with SMS code you must set your name:

```js
send('UpdateUser', {
  name: 'MyUser',
  photo: 'https://www.w3schools.com/w3images/avatar2.png'
  thumbnail: 'https://www.w3schools.com/w3images/avatar2.png',
  data: '{"age": 29}'
}, response => {
  // now auth is fully completed, we can show user main UI
});
```

All fields except `UpdateUser.name` are not required.

**NOTICE**: You can pass any user specific data through `UpdateUser.data`, which is a JSON-encoded string.

#### Get your profile

After successful authentication you can grab your profile:

```js
send('User', {}, response => {
  this.setProfile(response.user);
});
```

In this example `response.user` is `UserEntity` which backend always uses to describe you and `UserEntity.id` is your user ID.
Every other user in system will be a `ContactEntity` and `Contact.userId` will be his user ID.

### Contacts

Application should keep locally contact list that the user has [added to friends](#add-contact) and update this list.

Updates contact list occurs when you request [SyncContacts](#synchronize-contacts) or when you receive [ContactChanged](#event-contactChanged)

### Contact Statuses

Status | Description 
--- | ---
UNKNOWN | Initial state between users. When the user and his opponent did not add, block or add to spam each other
PENDING | When opponent add to contact user
FRIEND | When user add to contact opponent
BLOCKED | When user block opponent
I_AM_BLOCKED | When opponent block user
SPAMMER | When user add to contact opponent
I_AM_SPAMMER | When opponent add to spam user

### Add contact

User can add another user to contact list.

```js
send('AddContact', {ref: ref, contact: {userId: userId}}, response => {
  if (response typeof ContactResp) {
    this.upsertContact(response.contact);
  }
});
```

Possible ContactEntity.status transitions:

From | To 
--- | ---
UNKNOWN | FRIEND
PENDING | FRIEND

### Update contact

To rename Contact you can send a request:

```js
send('UpdateContact', {ref: ref, contact: {userId: userId, name: newName}}, response => {
  if (response typeof ContactResp) {
    this.upsertContact(response.contact);
  }
});
```

### Delete contact

Previously added contact can be deleted.

```js
send('DeleteContact', {ref: ref, userId: userId}, response => {
  if (response typeof ContactResp) {
    this.deleteContact(response.contact);
  }
});
```

**NOTICE**: Any Contact with ContactEntity.friend = true can be deleted.

Possible ContactEntity.status transitions:

From | To 
--- | ---
FRIEND | PENDING
FRIEND | UNKNOWN
BLOCKED | BLOCKED
I_AM_BLOCKED | I_AM_BLOCKED
SPAMMER | SPAMMER
I_AM_SPAMMER | I_AM_SPAMMER

### Block contact

To block any user 

```js
send('BlockContact', {ref: ref, userId: userId}, response => {
  if (response typeof ContactResp) {
    this.blockContact(response.contact);
  }
});
```

Messaging with blocked user are impossible.

Possible ContactEntity.status transitions:

From | To 
--- | ---
UNKNOWN | BLOCKED
PENDING | BLOCKED
FRIEND  | BLOCKED
I_AM_BLOCKED | BLOCKED

### UnBlock contact

You can unblock contact with the status BLOCKED

```js
send('UnBlockContact', {ref: ref, userId: userId}, response => {
  if (response typeof ContactResp) {
    this.unblockContact(response.contact);
  }
});
```

Possible ContactEntity.status transitions:

From | To 
--- | ---
BLOCKED | I_AM_BLOCKED
BLOCKED | FRIEND
BLOCKED | PENDING
BLOCKED | UNKNOWN

### Add to spam contact

Any user can be added to spam: 

```js
send('AddToSpam', {ref: ref, userId: userId}, response => {
  if (response typeof ContactResp) {
    this.deleteContact(response.contact);
  }
});
```

This user will no longer be able to write you messages, block and add to contact. The only one thing that user can do its add to spam you. You also can't text with them and change `ContactEntity.status`

**NOTICE**: This operation is not recoverable. 

Possible ContactEntity.status transitions:

From | To 
--- | ---
UNKNOWN | SPAMMER
PENDING | SPAMMER
FRIEND | SPAMMER
BLOCKED | SPAMMER
I_AM_BLOCKED | SPAMMER
I_AM_SPAMMER | SPAMMER

### Get contact

User can receive the ContactEntity by sending a request.

```js
send('Contact', {ref: ref, userId: userId}, response => {
  if (response typeof ContactResp) {
    // response.contact
  }
});
```

**NOTICE**: This request can be sent to get information in which relationship you are with the user. Even if this user is not your friend `ContactEntity.friend = false` (you never add him to contact) 

### Synchronize contacts

To synchronize your local contacts with server:

```js
send('SyncContacts', {ref: ref, syncTime: this.lastContactSyncTime}, response => {
  this.addOrUpdateContacts(response.contacts);
  // we save current server time to pass it to next 'SyncContacts' call
  this.lastContactSyncTime = response.serverTime;
});

this.addOrUpdateContacts(contacts) => {
  contacts.forEach(contactFromServer => {
    localContact = localContacts.find(contactFromServer);
    if (localContact) {
      if (contactFromServer.friend === false || contactFromServer.status === SPAMMER) {
        this.deleteContact(localContact)
      }
    } else if (contactFromServer.friend === true || contactFromServer.status !== SPAMMER) {
      this.upsertContact(contactFromServer);
    }
  })
}
```

Need to synchronize contact list when application was start or offline and keep `lastContactSyncTime`

In this example we specify `SyncContacts.syncTime` to get only new or updated contacts.

**NOTICE**: In response will be contacts that the user not added to his contact list. For example some user block you `ContactEntity.status = I_AM_BLOCKED`

### Event ContactChanged

When this message are received, the application should update the local contact list 
```js
ws.on('ContactChanged', contact => {
  this.addOrUpdateContacts([contact]);
});
```

**NOTICE**: `ContactChanged` receive via web socket.

## Messanging

After registration you have no chats. Chat with someone is created after sending first message, group chat is created by user. 

In our API we call private chat or room a "ChatUpdate" and it is represented by `ChatUpdateEntity`. In this guide we call private chat or room a "chat".

Start by looking at `ChatUpdateEntity` fields:

Field | Type | Description
--- | --- | ---
feedType | MessageFeedType | enum.MessageFeedType.{chat,group}
feedId | string | user ID or room ID
top | MessageEntity | last message in chat
room | RoomEntity | room is not empty only if its changed
hide | boolean | user hid this chat, client must hide this chat until new message arrives
delivered | timestamp | timestamp of last delivered to recipient message
seen | timestamp | timestamp of last message read by recipient
unread | integer | number of messages you didn't read
name | string | name of user or room topic
thumbnail | string | user or room thumbnail url
deleted | boolean | true if you: a) deleted chat (see `DeleteChat`); b) you quit from room; c) admin kicked you from room; d) or you deleted room
replyKeyboardMarkup | ReplyKeyboardMarkupEntity | see [Bots](#bots)
removeKeyboardMarkup | RemoveKeyboardMarkupEntity | see [Bots](#bots)
callId | string | see [Calls](#calls)

We use `feedType` and `feedId` as compound chat ID.

### Chats

Like with contacts, you can get all chats or new/updated chats only:

```js
send('ChatUpdates', {syncTime: this.lastChatUpdateSyncTime}, response => {
  response.updates.forEach(chat => this.createOrUpdateChat(chat));

  this.lastChatUpdateSyncTime = response.serverTime;
  this.timeCorrection = response.serverTime - Date.now();
});
```

We save difference between our and server time because we will need to know with high precision real timestamp of message that will be created on server before sending it to server to prevent UX troubles.

Clients must get chats on startup and after successful connect to WS.

After successful connect if something in chat changes backend sends you `ChatUpdate` message:

```js
ws.on('ChatUpdate', chatUpdateMsg => {
  this.createOrUpdateChat(chatUpdateMsg.update);
});
```

When you get `ChatUpdate` you should:

* sort and redraw last chats list (`ChatUpdate.{name,thumbnail,top,unread}`)
* hide chat if its deleted or hidden (`ChatUpdate.{hide,deleted}`)
* update room cache if `ChatUpdate.room` is not empty
* redraw delivered/seen markers (`ChatUpdate.{delivered,seen}`)
* if its room look at `Retrieve.callId`, maybe you have group call in progress

### Updates

When someone sends/updates/deletes message(s) backend adds new `UpdateEntity` to database and notifies all involved users with `Update` message:

```js
ws.on('Update', updateMsg => {
  this.processUpdate(updateMsg.update);
});

this.processUpdate = update => {
  const chat = this.findChat(update.feedType, update.feedId);

  if (update.type == enum.UpdateType.message || update.type == enum.UpdateType.updateMessage) {
    chat.createOrUpdateMessage(update.message);
  } else if (update.type == enum.UpdateType.deleteMessages) {
    chat.removeMessagesByIds(update.ids);
  }
};
```

To be sync with backend all client must call `Updates` to retrieve new updates from server on startup and for example once in a minute:

```js
this.syncUpdates = () => {
  //we pass last update id to get only new updates
  send('Updates', {stop: this.lastUpdateId}, response => {
    response.updates.forEach(update => this.processUpdate(update));
    this.lastUpdateId = response.updates[response.updates.length - 1].id;
  });
};
```

In this example `createOrUpdateMessage` method must update local message only if `created` of new message greater that `created` of local message. Also this method must sort messages in corresponding chat(s) and trigger UI to reflect changes.

**NOTICE**: `Updates` like many other messages uses top/stop/count to paginate (top - id of item to start with, stop - id of item to stop, count - limit max results or -1 to get rid of limit).

**NOTICE**: Updates are temporary, they just help us to be sure we are in sync with backend.

### Send message

Common practice on client is to create local version of message, send it to backend, wait for response from backend, remove local version of message and insert message received from backend:

```js
const localMessage = {
  id: 'message_id_' + Date.now(),
  feedType: enum.MessageFeedType.chat,
  feedId: '5c8173e6a255120001000025',
  payload: 'Hi mark',
  created: Date.now() + this.timeCorrection
};
this.createOrUpdateMessage(localMessage);

send('Message', {
  feedType: enum.MessageFeedType.chat,
  feedId: '5c8173e6a255120001000025',
  message: {
    kind: enum.MessageKind.text,
    payload: 'Hi mark'
  }
}, response => {
  this.removeMessageById(localMessage.id);
  this.createOrUpdateMessage(response.message);
});
```

In this example we send new text message to chat with some user.

All kinds of messages are explained in the following table that contains all fields you can put in `MessageEntity` when sending new message:

Field | Type | Description
--- | --- | ---
kind | MessageKind | enum.MessageKind.{text,image,video,audio,map,document,call}
payload | string | text of the message (use only with enum.MessageKind.text)
media | MediaEntity[] | media attachments (use only with enum.MessageKind.{image,video,audio,document})
geo | float[2] | lattitude and longitude (use only with enum.MessageKind.map)
replyId | string | not empty means this message is reply to other message
forwardId | string | not empty means this message is forwarded from another chat

### Retrieve messages

Backend provides you with `Retrieve` message to get message history for specific chat:

```js
send('Retrieve', {feedType: enum.MessageFeedType.room, feedId: '5cbc82c856601f0001000001', count: -1}, response => {
  // we got all messages in chat, cause count=-1 disables fetch limit
  const allMessagesInThisChat = response.message;
});
```

There are 3 scenarios when you need `Retrieve`:

1) You don't have local history for chat, so you need to get last N messages;
2) User just entered chat and you need to get all new messages since users last visit;
3) User scrolled up to the oldest message in local history and you need previous messages.

How to deal with these scenarios:

1) If you don't have local history for chat you must get last N messages, thats why we pass `Retrieve.count` but omit `Retrieve.{top,stop}`:

```js
send('Retrieve', {feedType: enum.MessageFeedType.room, feedId: '5cbc82c856601f0001000001', count: 50}, response => {
  response.messages.forEach(message => this.createOrUpdateMessage(message));
  //we need last message id for future Retrieve calls
  const lastMessageId = response.messages.last().id;
});
```

2) If you already have local history and user just entered chat, you must get all new messages till the newest item in cache, thats why we pass `Retrieve.count` = -1 to get rid of limits and `Retrieve.stop` to know where to stop:

```js
send('Retrieve', {
  feedType: enum.MessageFeedType.room,
  feedId: '5cbc82c856601f0001000001',
  stop: this.getLastMessageIdInChat(enum.MessageFeedType.room, '5cbc82c856601f0001000001'),
  count: -1
}, response => {
  if (response.messages) {
    response.messages.forEach(message => this.createOrUpdateMessage(message));
    this.setLastMessageIdInChat(enum.MessageFeedType.room, '5cbc82c856601f0001000001', response.messages.last().id);
  }
});
```

3) If user scrolled up to the oldest message in local history, you must get one more batch from server, thats why we pass `Retrieve.top` to start from our oldest message and take N previous messages:

```js
send('Retrieve', {
  feedType: enum.MessageFeedType.room,
  feedId: '5cbc82c856601f0001000001',
  top: this.getOldestMessageIdInChat(enum.MessageFeedType.room, '5cbc82c856601f0001000001'),
  count: 50
}, response => {
  response.messages.forEach(message => this.createOrUpdateMessage(message));
});
```

### Pointers

Backend uses `ChatUpdateEntity.{delivered,seen}` instead of status field for every single message, and we call them "pointers".

It means that you don't need to send pointers for every message. For example: if user has 5 unread messages in some chat, then when he opens chat you don't send 5 pointers for every message, you just send one pointer for the latest message:

```js
send('Pointer', {
  feedType: enum.MessageFeedType.chat,
  feedId: '5cbc82c856601f0001000001',
  seen: this.messages.last().created
}, () => {});
```

**NOTICE**: Client must store last sent pointers for every chat to don't send same pointers again and again.

Every user has his own `ChatUpdateEntity` for every chat, but be patient, backend saves pointers that YOU send to RECIPIENT's chat update, not to your chat update.

For example: You are logged in as user1 that has chat with user2. You send pointer, backend saves it to user2's chat update, user2 receives new pointer through `ChatUpdate` message and it triggers chat redraw to show pointers at corresponding messages in user2's client chat view.

**IMPORTANT**: For rooms delivered/seen are the SAME for ALL members.

#### Delivered

Delivered pointer is a date of the latest message that was received but not read by recipient(s).

Client must send delivered when new message arrives:

```js
ws.on('Update', updateMsg => {
  if (updateMsg.update.type == enum.UpdateType.message) {
    send('Pointer', {
      feedType: enum.MessageFeedType.chat,
      feedId: '5cbc82c856601f0001000001',
      delivered: updateMsg.message.created
    }, () => {});
  }
});
```

#### Seen

Seen pointer is a date of the latest message that was read by recipient(s).

Just send it when you are sure that user read everything until this message:

```js
const lastSentSeen = this.getLastSentSeen(enum.MessageFeedType.chat, '5cbc82c856601f0001000001');
if (lastSentSeen < someMessage.created) {
  send('Pointer', {
    feedType: enum.MessageFeedType.chat,
    feedId: '5cbc82c856601f0001000001',
    seen: someMessage.created
  }, () => {
    this.setLastSentSeen(enum.MessageFeedType.chat, '5cbc82c856601f0001000001', someMessage.created)
  });
}
```

**NOTICE**: In the example above we demonstrate how to don't send unnecessary `Pointer` messages to backend, but in real app this function will be much more complex than that.

## Calls

We use [Gruveo](https://www.gruveo.com) to make calls through WebRTC. In Calls section we talk about our signaling protocol.

First of all: Gruveo will ask to sign tokens, backend has message for that:

```js
send('SignGruveoToken', {token: 'some_token'}, response => {
  embed.authorize(response.signedToken);
});
```

### Dial

To make call you must send `InitCall`, backend will return you new unique `callId` for Gruveo.

```js
send('InitCall', {
  feedType: enum.MessageFeedType.chat,
  feedId: '5cbc82c856601f0001000001',
  video: true, //we want start video call
}, response => {
  this.initGruveoCall(response.callId);
})
```

Once you initiated call, backend sends `IncomingCall` to all participants of the chat:

```js
ws.on('IncomingCall', incomingCallMsg => {
  this.showIncomingCallView(incomingCallMsg.callId);
});
```

Also if its group call all participants will receive `ChatUpdateEntity` with not empty `callId` field:

```js
ws.on('ChatUpdate', chatUpdateMsg => {
  this.createOrUpdateChat(chatUpdateMsg.update);
  if (chatUpdateMsg.update.callId) {
    this.showIncomingCallView(chatUpdateMsg.update.callId);
  }
});
```

**NOTICE**: You must check, maybe call view for this callId is already on screen. We don't know what comes first `IncomingCall` or `ChatUpdate` message.

If after some time no one answers your call, you will receive `CallStatusChanged` with status `enum.CallStatus.timedOut`. Here is sample code that handles call status change:

```js
ws.on('CallStatusChanged', statusChangedMsg => {
  switch (statusChangedMsg.status) {
    case enum.CallStatus.accepted: this.showCallInProgressView(callId); break;
    case enum.CallStatus.rejected: this.showCallRejectedView(); break;
    case enum.CallStatus.timedOut: this.showCallTimedOutView(); break;
  }
});
```

**NOTICE**: You always know who is initiator of status change, cause backend puts his user ID in `CallStatusChanged.initiatorId`.

### Answer

If you have incoming call from someone, you can send your answer back:

```js
send('UpdateCallStatus', {
  feedType: enum.MessageFeedType.chat,
  feedId: '5cbc82c856601f0001000001',
  status: enum.CallStatus.accepted
}, response => {
  this.showCallInProgressView(callId);
});
```

You can answer only `enum.CallStatus.{accepted,rejected}` to incoming call. We use `enum.CallStatus.ended` to end call that you've already accepted.

### Hangup

To hang just update call status to `enum.CallStatus.ended`:

```js
send('UpdateCallStatus', {
  feedType: enum.MessageFeedType.chat,
  feedId: '5cbc82c856601f0001000001',
  status: enum.CallStatus.ended
}, response => {
  this.hideCallInProgressView();
});
```

But if you're in group call, you can join call again after you hang:

```js
const chatUpdate = this.getChatUpdate('5cbc82c856601f0001000001');
if (chatUpdate.callId) {
  send('UpdateCallStatus', {
    feedType: enum.MessageFeedType.room,
    feedId: '5cbc82c856601f0001000001',
    status: enum.CallStatus.accepted
  }, response => {
    this.showCallInProgressView(chatUpdate.callId);
  });
}
```

**NOTICE**: You must check that call is still in progress before join. Group call is in progress if `ChatUpdateEntity.callId` is not empty.

## Bots

Bots are regular users wit `ContactEntity.isBot` set to true.
Your app interacts with bots through our API.

In the following sections we describe messages that can be used and processed in chat with bot only.

### Bot metadata

Though bots are regular users backend provides you with some useful metadata about bot which is described by `BotEntity`.
Backend requires from client to send `Bots` message on app startup to get new and updated bots

```js
send('Bots', {syncTime: this.lastBotsSyncTime}, response => {
  this.createOrUpdateBots(response.bots);
  this.lastBotsSyncTime = response.serverTime;
});
```

and also send `Bot` message when user enters chat with bot:

```js
send('Bot', {userId: '5cbc82c856601f0001000001'}, response => {
  this.createOrUpdateBot(response.bot)
})
```

Lets take a look at `BotEntity` fields:

Field | Type | Description
--- | --- | ---
id | string | bot ID
userId | string | bot has own ID, thats why you have user ID here
username | string | -
descr | string | -
commands | BotCommandSetEntity[] | list of commands that bot supports grouped by user role (it prevents non admin users from seeing admin commands)
isInline | boolean | indicates that bot supports inline queries (see [Inline query](#inline-query))

**WARNING**: Bot's ID differs from bot's user ID! Client typically use bot ID only for local storage managment, everywhere else you work with bot's user ID.

We'll talk more about bot metadata later.

### Reply keyboard

Reply keyboard is additional keyboard with actions that are defined by bot.
Reply keyboard helps user to interact with bot quicker and to understand what bot can do.

In chat with bot user can switch between text input keyboard and reply keyboard.
Reply keyboard must be rendered as table with equal column widhts that fits container width. Each table cell contains one button.

In our API reply keyboard is represented by `ReplyKeyboardMarkupEntity`. It has field `buttonRows` which contains rows of the keyboard represented by `KeyboardButtonRowEntity`. It has field `buttons`, which contains buttons of the row represented by `KeyboardButtonEntity`.

When bot sends you new reply keyboard you get `ChatUpdate` with not empty `ChatUpdateEntity.replyKeyboardMarkup` and also you will get updated `ChatUpateEntity` when you get chat updates. You must show/redraw/hide reply keyboard in both cases.

There is one more field that is opposite to `ChatUpdateEntity.replyKeyboardMarkup`, its `ChatUpdateEntity.removeKeyboardMarkup`. If `ChatUpdateEntity.removeKeyboardMarkup` is not empty, than you must hide reply keyboard and button that show it.

Lets take a look at `KeyboardButtonEntity` fields:

Field | Type | Description
--- | --- | ---
text | string | text on button
payload | string | text that you must send to bot as text message on tap (if empty, send `text` instead)
requestLocation | boolean | indicates that you must get location from user and send it as map message

**NOTICE**: The best way to handle `KeyboardButtonEntity.requestLocation` is to show user a map with a marker at his current position. User can change marker position if he wants to and tap "send" or "cancel".

### Inline keyboard

Inline keyboard is similar to reply keyboard, but it appers not in text input area, inline keyboard can be attached to any message in chat (see `MessageEntity.inlineKeyboardMarkup`). Bots send messages with inline keyboard to give user fixed number of reply options.

In our API inline keyboard is represented by `InlineKeyboardMarkupEntity`. It has field `buttonRows` which contains rows of the keyboard represented by `InlineKeyboardButtonRowEntity`. It has field `buttons`, which contains buttons of the row represented by `InlineKeyboardButtonEntity`.

Field | Type | Description
--- | --- | ---
text | string | text on button
url | string | if not empty, show url-styled button istead of button with text
callbackData | string | data that you must send to bot on tap

If `InlineKeyboardButtonEntity.url` is not empty, then just open browser when user taps the button (for mobile clients WebView is preferable), otherwise you must send users answer to bot:

```js
send('BotCallbackQuery', {
  messageId: '5cbc82c856601f0001000001',
  data: inlineButton.callbackData
}, response => {});
```

Bot will process your answer and react somehow.

### Inline queries

Inline queries is a way to implement search through bot. For example you can search wikipedia articles through Wiki Bot. User initiates search using text message input by typing "@" followed by bot username followed by space followed by search query. For example: `@wiki world war`. In this example `wiki` is bot's username and `world war` is a search query.

**NOTICE**: Its client's responsibility to parse text message input while user is typing and understand whether its inline query or not. You need bots metadata to understand if user wants to mention other user in message or user wants send search query to bot.

User can send inline queries to any bot in any chat but only bots that support inline queries has `BotEntity.isInline` set to true.

Now lets see how to send search query (imagine that user entered @wiki world war):

```js
send('BotInlineQuery', {
  feedType: map.MessageFeedType.chat,
  feedId: '5cbc82c856601f0001000001',
  queryId: 'xcR8SxseWxv',
  query: '@wiki world war' //here "wiki" is `BotEntity.username`
}, response => {});
```

**NOTICE**: You must generate `queryId` to identify answer from bot when it comes.

After processing bot will send you search results in form of `BotInlineQueryAnswer` message:

```js
ws.on('BotInlineQueryAnswer', response => {
  this.renderInlineQueryResults(response.results);
  this.nextInlineQueryOffset = response.nextOffset;
});
```

In this example we save `BotInlineQueryAnswer.nextOffset` to get next page from bot:

```js
send('BotInlineQuery', {
  feedType: map.MessageFeedType.chat,
  feedId: '5cbc82c856601f0001000001',
  queryId: 'xcR8SxseWxv',
  query: '@wiki world war',
  offset: this.nextInlineQueryOffset
}, response => {});
```

As you see in our second query all fields remain the same except `BotInlineQuery.offset`.

Structure of `BotInlineQueryAnswer`:

Field | Type | Description
--- | --- | ---
feedType | string | -
feedId | string | -
userId | string | bot's user ID
type | InlineQueryResultType | article, image, etc (see `InlineQueryResultType`)
queryId | string | generated by client unique query id
results | MediaEntity[] | results to display
nextOffset | string | pass this value to `BotInlineQuery.offset` to get next page

Finally you can use result chosen by user to attach it to message and send.
