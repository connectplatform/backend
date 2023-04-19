module.exports = function(p){

	const user3 = {
		id: '000000000000000000000003',
		delivered: 0,
		seen: 0,
		unread: 0
	};
	const user4 = {
		id: '000000000000000000000004',
		delivered: 0,
		seen: 0,
		unread: 0
	};
	someNotTrackedUserId = '000000000000000000000005';

  p
  .connect('user3')
  .connect('user4')
	.set('beforeRoomCreateTime', +new Date())
  .send('user3', 'Room', {room: {topic: 'Room 1', members: [user4.id, someNotTrackedUserId]}}, response => {
    p.equals(response.modelName, 'RoomResp');
    p.set('roomId', response.room.id);
    user3.unread++;
    user4.unread++;
  })
  //user3 sends message
  .then()
  .send('user3', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.room, get('roomId'), 'Message 1'), response => {
    p.equals(response.modelName, 'MessageResp');
    p.set('messageId1', response.message.id);
    p.set('messageCreated1', response.message.created);
  })
  .and()
  .wait('user3', 'Update', response => {
    p.pass()
  })
  .and()
  .wait('user4', 'Update', response => {
    p.pass()
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room);
    p.equals(response.update.feedId, get('roomId'));
    p.equals(response.update.delivered, user3.delivered);
    p.equals(response.update.seen, user3.seen);
    p.equals(response.update.unread, user3.unread);
  })
  .and()
  .wait('user4', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room);
    p.equals(response.update.feedId, get('roomId'));
    p.equals(response.update.delivered, user4.delivered);
    p.equals(response.update.seen, user4.seen);
    p.equals(response.update.unread, ++user4.unread);
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedId, get('roomId'))
    p.equals(response.updates[0].top.payload, 'Message 1')
    p.equals(response.updates[0].delivered, user3.delivered)
    p.equals(response.updates[0].seen, user3.seen)
    p.equals(response.updates[0].unread, user3.unread)
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].feedId, get('roomId'));
    p.equals(response.updates[0].top.payload, 'Message 1');
    p.equals(response.updates[0].delivered, user4.delivered);
    p.equals(response.updates[0].seen, user4.seen);
    p.equals(response.updates[0].unread, user4.unread);
  })
  //user4 sends delivered pointer
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.room, feedId: get('roomId'), delivered: get('messageCreated1')}, response => {
    p.equals(response.modelName, 'PointerResp');
		user3.delivered = get('messageCreated1');
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
		user3.delivered = get('messageCreated1');
    p.equals(response.update.delivered, user3.delivered);
    p.equals(response.update.seen, user3.seen);
    p.equals(response.update.unread, user3.unread);
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].delivered, user3.delivered);
    p.equals(response.updates[0].seen, user3.seen);
    p.equals(response.updates[0].unread, user3.unread);
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].delivered, user4.delivered);
    p.equals(response.updates[0].seen, user4.seen);
    p.equals(response.updates[0].unread, user4.unread);
  })
  //check user3 can't send seen pointer to own message
  .then()
  .send('user3', 'Pointer', {feedType: map.MessageFeedType.room, feedId: get('roomId'), seen: get('messageCreated1')}, response => {
    p.equals(response.modelName, 'PointerResp');
		user3.unread--;
		user4.delivered = p.get('messageCreated1');
		user4.seen = p.get('messageCreated1');
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].delivered, user3.delivered);
    p.equals(response.updates[0].seen, user3.seen);
    p.equals(response.updates[0].unread, user3.unread);
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].delivered, user4.delivered);
    p.equals(response.updates[0].seen, user4.seen);
    p.equals(response.updates[0].unread, user4.unread);
  })
  //send one more message user3 -> user4
  .then()
  .send('user3', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.room, get('roomId'), 'Message 2'), response => {
    p.equals(response.modelName, 'MessageResp');
    p.set('messageId2', response.message.id);
    p.set('messageCreated2', response.message.created);
    user4.unread++;
  })
  .and()
  .wait('user4', 'Update', response => {
    p.pass()
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].top.payload, 'Message 2');
    p.equals(response.updates[0].delivered, user3.delivered);
    p.equals(response.updates[0].seen, user3.seen);
    p.equals(response.updates[0].unread, user3.unread);
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].top.payload, 'Message 2');
    p.equals(response.updates[0].delivered, user4.delivered);
    p.equals(response.updates[0].seen, user4.seen);
    p.equals(response.updates[0].unread, user4.unread);
  })
  //user4 sends delivered to last message
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.room, feedId: get('roomId'), delivered: get('messageCreated2')}, response => {
    p.equals(response.modelName, 'PointerResp');
    user3.delivered = p.get('messageCreated2');
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
		user3.delivered = p.get('messageCreated2');
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.equals(response.update.delivered, user3.delivered)
    p.equals(response.update.seen, user3.seen)
    p.equals(response.update.unread, user3.unread)
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, user3.delivered)
    p.equals(response.updates[0].seen, user3.seen)
    p.equals(response.updates[0].unread, user3.unread)
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, user4.delivered)
    p.equals(response.updates[0].seen, user4.seen)
    p.equals(response.updates[0].unread, user4.unread)
  })
  //check that older seen doesn't affect unread
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.room, feedId: get('roomId'), seen: get('beforeRoomCreateTime')}, response => {
    p.equals(response.modelName, 'PointerResp');
    user3.seen = p.get('beforeRoomCreateTime');
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].delivered, user3.delivered);
    p.equals(response.updates[0].seen, user3.seen);
    p.equals(response.updates[0].unread, user3.unread);
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, user4.delivered)
    p.equals(response.updates[0].seen, user4.seen)
    p.equals(response.updates[0].unread, user4.unread)
  })
  //user4 marks first message read
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.room, feedId: get('roomId'), seen: get('messageCreated1')}, response => {
    p.equals(response.modelName, 'PointerResp');
		user3.seen = p.get('messageCreated1');
		user4.unread = user4.unread - 2;
  })
	.then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates[0].delivered, user3.delivered);
    p.equals(response.updates[0].seen, user3.seen);
    p.equals(response.updates[0].unread, user3.unread);
  })
	.then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates[0].delivered, user4.delivered);
    p.equals(response.updates[0].seen, user4.seen);
    p.equals(response.updates[0].unread, user4.unread);
  })
  //user4 marks secind message read
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.room, feedId: get('roomId'), seen: get('messageCreated2')}, response => {
    p.equals(response.modelName, 'PointerResp');
		user3.seen = p.get('messageCreated2');
		user4.unread = 0;
  })
	.then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates[0].delivered, user3.delivered);
    p.equals(response.updates[0].seen, user3.seen);
    p.equals(response.updates[0].unread, user3.unread);
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates[0].delivered, user4.delivered);
    p.equals(response.updates[0].seen, user4.seen);
    p.equals(response.updates[0].unread, user4.unread);
  })

  //check that older seen doesn't affect unread once more
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.room, feedId: get('roomId'), seen: get('messageCreated1')}, response => {
    p.equals(response.modelName, 'PointerResp');
    // console.log("messageCreated1:", get('messageCreated1'));
    // console.log("messageCreated2:", get('messageCreated2'));
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
		p.equals(response.updates[0].delivered, user3.delivered);
		p.equals(response.updates[0].seen, user3.seen);
		p.equals(response.updates[0].unread, user3.unread);
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
		p.equals(response.updates[0].delivered, user4.delivered);
		p.equals(response.updates[0].seen, user4.seen);
		p.equals(response.updates[0].unread, user4.unread);
  })

  //check that edit message causes chat update
  .then()
  .send('user3', 'EditMessage', {message: {id: get('messageId2'), payload: 'Message 2 Edited'}}, response => {
    p.equals(response.modelName, 'EditMessageResp');
    p.equals(response.message.id, get('messageId2'));
    p.equals(response.message.payload, 'Message 2 Edited')
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].top.payload, 'Message 2 Edited');
		p.equals(response.updates[0].delivered, user3.delivered);
		p.equals(response.updates[0].seen, user3.seen);
		p.equals(response.updates[0].unread, user3.unread);
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
    p.equals(response.updates[0].top.payload, 'Message 2 Edited');
		p.equals(response.updates[0].delivered, user4.delivered);
		p.equals(response.updates[0].seen, user4.seen);
		p.equals(response.updates[0].unread, user4.unread);
  })

  //check that delete message causes unread recalculation
  .then()
  .send('user3', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.room, get('roomId'), 'Message 3'), response => {
    p.equals(response.modelName, 'MessageResp');
    p.set('messageId3', response.message.id);
		user4.unread++;
  })
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1);
		p.equals(response.updates[0].delivered, user4.delivered);
		p.equals(response.updates[0].seen, user4.seen);
		p.equals(response.updates[0].unread, user4.unread);
  })
  .then()
  .send('user4', 'Delete', {feedType: map.MessageFeedType.room, feedId: get('roomId'), ids: [get('messageId3')]}, response => {
    p.equals(response.modelName, 'DeleteResp');
		user4.unread--;
  })
	.then()
	.send('user4', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates[0].feedType, map.MessageFeedType.room);
    p.equals(response.updates[0].feedId, get('roomId'));
    p.equals(response.updates[0].top.payload, 'Message 2 Edited');
		p.equals(response.updates[0].delivered, user4.delivered);
		p.equals(response.updates[0].seen, user4.seen);
		p.equals(response.updates[0].unread, user4.unread);
  })
};
