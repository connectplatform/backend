module.exports = function (p) {
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
	const feedTypeChat = map.MessageFeedType.chat;
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

	p
	.connect('user1')
  .connect('user2')
	.then()
	// send 1 message user1 -> user2
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 1'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.equals(response.message.userTime, 1500000000);
		p.set('firstMessage.id', response.message.id);
		p.set('firstMessage.created', response.message.created);
		user2.unread++;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.then()
	// send 10 messages user1 -> user2
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 2'), () => {user2.unread++})
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 3'), () => {user2.unread++})
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 4'), () => {user2.unread++})
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 5'), () => {user2.unread++})
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 6'), () => {user2.unread++})
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 7'), () => {user2.unread++})
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 8'), () => {user2.unread++})
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 9'), () => {user2.unread++})
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 10'), () => {user2.unread++})
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'Message 11'), () => {user2.unread++})
	.then(responses => {
		const response = responses.find(r => r.message.created === Math.max.apply(null, responses.map(r => r.message.created)));
		p.set('afterTenMessageSend', response.message.created);
	})
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.then()
	//  send delivered first message user2 -> user1
	.send('user2', 'Pointer', {feedType: feedTypeChat, feedId: user1.id, delivered: get('firstMessage.created'), seen: 0}, response => {
		p.equals(response.modelName, 'PointerResp');
		user1.delivered = p.get('firstMessage.created');
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.then()
	//  send seen first message user2 -> user1
	.send('user2', 'Pointer', {feedType: feedTypeChat, feedId: user1.id, delivered: 0, seen: get('firstMessage.created')}, response => {
		p.equals(response.modelName, 'PointerResp');
		user1.seen = p.get('firstMessage.created');
		user2.unread--;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	// user1 delete top message
	.then()
	// send 1 message user1 -> user2
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'some message'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.set('someMessage.id', response.message.id);
		p.set('someMessage.created', response.message.created);
		user2.unread++;
	})
	.then()
	// send 1 message user1 -> user2
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'before top message'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.set('beforeTopMessage.id', response.message.id);
		p.set('beforeTopMessage.created', response.message.created);
		user2.unread++;
	})
	.then()
	// send 1 message user1 -> user2
	.send('user1', 'Message', global.entityHelper.generateMessage(feedTypeChat, user2.id, 'top message'), response => {
		p.equals(response.modelName, 'MessageResp');
		p.set('topMessage.id', response.message.id);
		p.set('topMessage.created', response.message.created);
		user2.unread++;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	// Delete top unread message
	.then()
	.send('user1', 'Delete', {feedType: feedTypeChat, feedId: user2.id, ids: [get('topMessage.id')]}, response => {
		p.equals(response.modelName, 'DeleteResp');
		user2.unread--;
	})
	.then()
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	// Delete some unread message
	.then()
	.send('user1', 'Delete', {feedType: feedTypeChat, feedId: user2.id, ids: [get('someMessage.id')]}, response => {
		p.equals(response.modelName, 'DeleteResp');
		user2.unread--;
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	.then()
	.send('user1', 'EditMessage', {message: {id: get('firstMessage.id'), payload: 'Message 1 Edited'}}, response => {
		p.equals(response.modelName, 'EditMessageResp');
		p.equals(response.message.id, get('firstMessage.id'));
		p.equals(response.message.payload, 'Message 1 Edited')
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
	// Test the pointer can't move back
	.then()
	.send('user2', 'Pointer', {feedType: feedTypeChat, feedId: user1.id, delivered: 0, seen: 1}, response => {
		p.equals(response.modelName, 'PointerResp');
	})
	.and()
	.send('user1', 'Pointer', {feedType: feedTypeChat, feedId: user2.id, delivered: 0, seen: 1}, response => {
		p.equals(response.modelName, 'PointerResp');
	})
	.then()
	.send('user1', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser1)
	.send('user2', 'ChatUpdates', {syncTime: 0}, matchChatUpdateResponseForUser2)
};
