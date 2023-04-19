module.exports = function(p){
  p
  .connect('user3')
  .connect('user4')
  //send message user3 -> user4
  .send('user3', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000004', 'Message 1'), response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId1', response.message.id)
    p.set('messageCreated1', response.message.created)
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
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000004')
    p.equals(response.update.top.payload, 'Message 1')
    p.equals(response.update.delivered, 0)
    p.equals(response.update.seen, 0)
    p.equals(response.update.unread, 0)
  })
  .and()
  .wait('user4', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000003')
    p.equals(response.update.top.payload, 'Message 1')
    p.equals(response.update.delivered, 0)
    p.equals(response.update.seen, 0)
    p.equals(response.update.unread, 1)
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedId, '000000000000000000000004')
    p.equals(response.updates[0].top.payload, 'Message 1')
    p.equals(response.updates[0].delivered, 0)
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 0)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedId, '000000000000000000000003')
    p.equals(response.updates[0].top.payload, 'Message 1')
    p.equals(response.updates[0].delivered, 0)
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 1)
  })
  //user4 sends delivered pointer
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000003', delivered: get('messageCreated1')}, response => {
    p.equals(response.modelName, 'PointerResp')
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.delivered, get('messageCreated1'))
    p.equals(response.update.seen, 0)
    p.equals(response.update.unread, 0)
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, get('messageCreated1'))
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 0)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, 0)
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 1)
  })
  //check user3 can't send seen pointer to own message
  .then()
  .send('user3', 'Pointer', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000004', seen: get('messageCreated1')}, response => {
    p.equals(response.modelName, 'PointerResp')
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, get('messageCreated1'))
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 0)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, 0)
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 1)
  })
  //send one more message user3->user4
  .then()
  .send('user3', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000004', 'Message 2'), response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId2', response.message.id)
    p.set('messageCreated2', response.message.created)
  })
  .and()
  .wait('user4', 'Update', response => {
    p.pass()
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].top.payload, 'Message 2')
    p.equals(response.updates[0].delivered, get('messageCreated1'))
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 0)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].top.payload, 'Message 2')
    p.equals(response.updates[0].delivered, 0)
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 2)
  })
  //user4 sends delivered to last message
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000003', delivered: get('messageCreated2')}, response => {
    p.equals(response.modelName, 'PointerResp')
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.delivered, get('messageCreated2'))
    p.equals(response.update.seen, 0)
    p.equals(response.update.unread, 0)
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, get('messageCreated2'))
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 0)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, 0)
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 2)
  })
  //check that older seen doesn't affect unread
  .then()
  .send('user4', 'Pointer', {
    feedType: map.MessageFeedType.chat,
    feedId: '000000000000000000000003',
    seen: get('messageCreated1', key => get(key) - 1000)
  }, response => {
    p.equals(response.modelName, 'PointerResp')
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, get('messageCreated2'))
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 0)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, 0)
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 2)
  })
  //user4 marks first message read
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000003', seen: get('messageCreated1')}, response => {
    p.equals(response.modelName, 'PointerResp')
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000004')
    p.equals(response.update.delivered, get('messageCreated2'))
    p.equals(response.update.seen, get('messageCreated1'))
    p.equals(response.update.unread, 0)
  })
  .and()
  .wait('user4', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000003')
    p.equals(response.update.delivered, 0)
    p.equals(response.update.seen, 0)
    p.equals(response.update.unread, 1)
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].seen, get('messageCreated1'))
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].unread, 1)
  })
  //user4 marks second message read
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000003', seen: get('messageCreated2')}, response => {
    p.equals(response.modelName, 'PointerResp')
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000004')
    p.equals(response.update.delivered, get('messageCreated2'))
    p.equals(response.update.seen, get('messageCreated2'))
    p.equals(response.update.unread, 0)
  })
  .and()
  .wait('user4', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000003')
    p.equals(response.update.delivered, 0)
    p.equals(response.update.seen, 0)
    p.equals(response.update.unread, 0)
  })
  //check that older seen doesn't affect unread once more
  .then()
  .send('user4', 'Pointer', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000003', seen: get('messageCreated1')}, response => {
    p.equals(response.modelName, 'PointerResp')
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, get('messageCreated2'))
    p.equals(response.updates[0].seen, get('messageCreated2'))
    p.equals(response.updates[0].unread, 0)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, 0)
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 0)
  })
  //check that edit message causes chat update
  .then()
  .send('user3', 'EditMessage', {message: {id: get('messageId2'), payload: 'Message 2 Edited'}}, response => {
    p.equals(response.modelName, 'EditMessageResp')
    p.equals(response.message.id, get('messageId2'))
    p.equals(response.message.payload, 'Message 2 Edited')
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000004')
    p.equals(response.update.top.payload, 'Message 2 Edited')
    p.equals(response.update.unread, 0)
  })
  .and()
  .wait('user4', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000003')
    p.equals(response.update.top.payload, 'Message 2 Edited')
    p.equals(response.update.unread, 0)
  })
  //check that delete message causes unread recalculation
  .then()
  .send('user3', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000004', 'Message 3'), response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId3', response.message.id)
    p.set('messageCreated3', response.message.created)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].delivered, 0)
    p.equals(response.updates[0].seen, 0)
    p.equals(response.updates[0].unread, 1)
  })
  .then()
  .send('user3', 'Delete', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000004', ids: [get('messageId3')]}, response => {
    p.equals(response.modelName, 'DeleteResp')
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000004')
    // p.e_quals(response.update.top.payload, 'Message 2 Edited') //TODO: fix me!
    p.equals(response.update.unread, 0)
  })
  .and()
  .wait('user4', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000003')
    // p.e_quals(response.update.top.payload, 'Message 2 Edited') //TODO: fix me!
    p.equals(response.update.unread, 0)
  })
  //check sending feedId (get only update for specified feedId)
  .then()
  .send('user4', 'ChatUpdates', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000003'}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000003')
    p.equals(response.updates[0].unread, 0)
  })
  //check mark as read works
  .then()
  .send('user4', 'MarkAsRead', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000003'}, response => {
    p.equals(response.modelName, 'MarkAsReadResp')
  })
  // TODO: ensure chat update arrives
  .then()
  .send('user4', 'ChatUpdates', {syncTime: 0, feedType: map.MessageFeedType.chat, feedId: '000000000000000000000003'}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000003')
    p.equals(response.updates[0].delivered > get('messageCreated3'), true)
    p.equals(response.updates[0].seen > get('messageCreated3'), true)
    p.equals(response.updates[0].unread, 0)
  })
  //check delete chat
  .then()
  .send('user4', 'DeleteChat', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000003'}, response => {
    p.equals(response.modelName, 'DeleteChatResp')
  })
  .and()
  .wait('user4', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000003')
    p.equals(response.update.deleted, true)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].deleted, true)
  })
  //check that send message to deleted chat makes it not deleted
  .then()
  .send('user3', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000004', 'Message 5'), response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId5', response.message.id)
  })
  .and()
  .wait('user4', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000003')
    p.equals(response.update.deleted, false)
  })
  .then()
  .send('user4', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].top.id, get('messageId5'))
    p.equals(response.updates[0].deleted, false)
  })
}
