module.exports = function(p){
	const user1 = {
		id: '000000000000000000000001',
		delivered: 0,
		seen: 0,
		unread: 0
	};
	const user2 = {
		id: '000000000000000000000002',
		delivered: 0,
		seen: 0,
		unread: 0
	};
	const user3 = {
		id: '000000000000000000000003',
		delivered: 0,
		seen: 0,
		unread: 0
	};
	const feedTypeRoom = map.MessageFeedType.room;
	const matchChatUpdateResponseForUser1 = (response) => {
		p.equals(response.updates[0].seen, user1.seen);
		p.equals(response.updates[0].delivered, user1.delivered);
		p.equals(response.updates[0].unread, user1.unread);
	};
	const matchChatUpdateResponseForUser2 = (response) => {
		p.equals(response.updates[0].seen, user2.seen);
		p.equals(response.updates[0].delivered, user2.delivered);
		p.equals(response.updates[0].unread, user2.unread);
	};
	const matchChatUpdateResponseForUser3 = (response) => {
		p.equals(response.updates[0].seen, user3.seen);
		p.equals(response.updates[0].delivered, user3.delivered);
		p.equals(response.updates[0].unread, user3.unread);
	};

  p
  .connect('user1')
  .connect('user2')
  .connect('user3')
	.set('beforeRoomCreateTime', +new Date())
	// create room
	.then()
	.send('user1', 'Room', {room: {topic: 'Room', members: [user2.id, user3.id]}}, response => {
		p.equals(response.modelName, 'RoomResp');
		p.set('roomId', response.room.id);
		// user create room system message
		user1.unread++;
		user2.unread++;
		user3.unread++;
	})
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeRoom, get('roomId'), '[user1] Message 1'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.set('user1.message1.id', response.message.id);
		p.set('user1.message1.created', response.message.created);
		user2.unread++;
		user3.unread++;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user3', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser3)
	.then()
	.send('user2', 'Message', global.entityHelper.generateMessage(feedTypeRoom, get('roomId'), '[user2] Message 1'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.set('user2.message1.id', response.message.id);
		p.set('user2.message1.created', response.message.created);
		user1.unread++;
		user3.unread++;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user3', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser3)
	.then()
	.send('user3', 'Message', global.entityHelper.generateMessage(feedTypeRoom, get('roomId'), '[user3] Message 1'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.set('user3.message1.id', response.message.id);
		p.set('user3.message1.created', response.message.created);
		user1.unread++;
		user2.unread++;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user3', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser3)
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeRoom, get('roomId'), '[user1] Message 2'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.set('user1.message2.id', response.message.id);
		p.set('user1.message2.created', response.message.created);
		user2.unread++;
		user3.unread++;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user3', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser3)
	.then()
	.send('user1', 'Delete', {feedType: feedTypeRoom, feedId: get('roomId'), ids: [get('user1.message1.id'), get('user1.message2.id')]}, response => {
		p.equals(response.modelName, 'DeleteResp');
		user2.unread = user2.unread - 2;
		user3.unread = user3.unread - 2;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user3', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser3)
	.then()
	.send('user2', 'Pointer', {feedType: feedTypeRoom, feedId: get('roomId'), delivered: 0, seen: get('user3.message1.created')}, response => {
		p.equals(response.modelName, 'PointerResp');

		user1.delivered = p.get('user3.message1.created');
		user1.seen = p.get('user3.message1.created');

		user2.unread = 0; // must be 0 because message 'user1.message2.created' is deleted

		user3.delivered = p.get('user3.message1.created');
		user3.seen = p.get('user3.message1.created');
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user3', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser3)
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeRoom, get('roomId'), '[user1] Message 3'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.set('user1.message3.id', response.message.id);
		p.set('user1.message3.created', response.message.created);
		user2.unread++;
		user3.unread++;
	})
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeRoom, get('roomId'), '[user1] Message 4'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.set('user1.message4.id', response.message.id);
		p.set('user1.message4.created', response.message.created);
		user2.unread++;
		user3.unread++;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user3', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser3)
	.then()
	.send('user1', 'Pointer', {feedType: feedTypeRoom, feedId: get('roomId'), delivered: 0, seen: get('beforeRoomCreateTime')}, response => {
		p.equals(response.modelName, 'PointerResp');
		user2.seen = user2.delivered = p.get('beforeRoomCreateTime');
	})
	.then()
	.send('user2', 'Pointer', {feedType: feedTypeRoom, feedId: get('roomId'), delivered: 0, seen: get('beforeRoomCreateTime')}, response => {
		p.equals(response.modelName, 'PointerResp');
	})
	.then()
	.send('user3', 'Pointer', {feedType: feedTypeRoom, feedId: get('roomId'), delivered: 0, seen: get('beforeRoomCreateTime')}, response => {
		p.equals(response.modelName, 'PointerResp');
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user3', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser3)
	.then()
	.send('user2', 'EditMessage', {message: {id: get('user2.message1.id'), payload: 'Message Edited'}}, response => {
		p.equals(response.modelName, 'EditMessageResp');
		p.equals(response.message.id, get('user2.message1.id'));
		p.equals(response.message.payload, 'Message Edited');
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user3', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser3)
};
