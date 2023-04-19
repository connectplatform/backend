module.exports = function(p){
  p
  .connect('user1')
  .connect('user2')
  //new message
  .then()
  .send('user2', 'Message', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000001', message: {payload: 'Message 1'}}, response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId1', response.message.id)
  })
  .and()
  .wait('user1', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.message)
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000002')
    p.equals(response.update.message.payload, 'Message 1')
  })
  .then()
  .send('user2', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000001')
    p.equals(response.updates[0].top.id, get('messageId1'))
    p.equals(response.updates[0].top.payload, 'Message 1')
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000002')
    p.equals(response.updates[0].top.id, get('messageId1'))
    p.equals(response.updates[0].top.payload, 'Message 1')
  })
  .then()
  .send('user2', 'Updates', {count: -1}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].type, map.UpdateType.message)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000001')
    p.equals(response.updates[0].message.id, get('messageId1'))
    p.equals(response.updates[0].message.payload, 'Message 1')
  })
  .then()
  .send('user1', 'Updates', {count: -1}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].type, map.UpdateType.message)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000002')
    p.equals(response.updates[0].message.id, get('messageId1'))
    p.equals(response.updates[0].message.payload, 'Message 1')
  })

  //edit message
  .then()
  .send('user2', 'EditMessage', {message: {id: get('messageId1'), payload: 'Message 1 edited'}}, response => {
    p.equals(response.modelName, 'EditMessageResp')
    p.equals(response.message.id, get('messageId1'))
    p.equals(response.message.payload, 'Message 1 edited')
  })
  .and()
  .wait('user1', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.updateMessage)
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000002')
    p.equals(response.update.message.payload, 'Message 1 edited')
  })
  .then()
  .send('user2', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000001')
    p.equals(response.updates[0].top.id, get('messageId1'))
    p.equals(response.updates[0].top.payload, 'Message 1 edited')
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000002')
    p.equals(response.updates[0].top.id, get('messageId1'))
    p.equals(response.updates[0].top.payload, 'Message 1 edited')
  })
  .then()
  .send('user2', 'Updates', {count: -1}, response => {
    p.equals(response.updates.length, 2)

    p.equals(response.updates[0].type, map.UpdateType.message)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000001')
    p.equals(response.updates[0].message.id, get('messageId1'))
    p.equals(response.updates[0].message.payload, 'Message 1 edited')

    p.equals(response.updates[1].type, map.UpdateType.updateMessage)
    p.equals(response.updates[1].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[1].feedId, '000000000000000000000001')
    p.equals(response.updates[1].message.id, get('messageId1'))
    p.equals(response.updates[1].message.payload, 'Message 1 edited')
  })
  .then()
  .send('user1', 'Updates', {count: -1}, response => {
    p.equals(response.updates.length, 2)

    p.equals(response.updates[0].type, map.UpdateType.message)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000002')
    p.equals(response.updates[0].message.id, get('messageId1'))
    p.equals(response.updates[0].message.payload, 'Message 1 edited')

    p.equals(response.updates[1].type, map.UpdateType.updateMessage)
    p.equals(response.updates[1].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[1].feedId, '000000000000000000000002')
    p.equals(response.updates[1].message.id, get('messageId1'))
    p.equals(response.updates[1].message.payload, 'Message 1 edited')
  })

  //delete the only message in chat
  .then()
  .send('user2', 'Delete', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000001', ids: [get('messageId1')]}, response => {
    p.equals(response.modelName, 'DeleteResp')
  })
  .and()
  .wait('user1', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.deleteMessages)
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000002')
    p.deepEquals(response.update.ids, [get('messageId1')])
    p.equals(response.update.message, null)
  })
  .then()
  .send('user2', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000001')
    p.equals(response.updates[0].top, null)
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000002')
    p.equals(response.updates[0].top, null)
  })
  .then()
  .send('user2', 'Updates', {count: -1}, response => {
    p.equals(response.updates.length, 3)

    p.equals(response.updates[0].type, map.UpdateType.message)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000001')
    p.equals(response.updates[0].message.payload, 'Message 1 edited')

    p.equals(response.updates[1].type, map.UpdateType.updateMessage)
    p.equals(response.updates[1].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[1].feedId, '000000000000000000000001')
    p.equals(response.updates[1].message.payload, 'Message 1 edited')

    p.equals(response.updates[2].type, map.UpdateType.deleteMessages)
    p.equals(response.updates[2].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[2].feedId, '000000000000000000000001')
    p.deepEquals(response.updates[2].ids, [get('messageId1')])
    p.equals(response.updates[2].message, null)
  })
  .then()
  .send('user1', 'Updates', {count: -1}, response => {
    p.equals(response.updates.length, 3)
    p.equals(response.updates[0].type, map.UpdateType.message)
    p.equals(response.updates[1].type, map.UpdateType.updateMessage)
    p.equals(response.updates[2].type, map.UpdateType.deleteMessages)
  })

  //delete not my message
  .then()
  .send('user2', 'Message', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000001', message: {payload: 'Message 2'}}, response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId2', response.message.id)
  })
  .then()
  .send('user1', 'Delete', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000002', ids: [get('messageId2')]}, response => {
    p.equals(response.modelName, 'DeleteResp')
  })
  .then()
  .send('user2', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000001')
    p.equals(response.updates[0].top.id, get('messageId2'))
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, '000000000000000000000002')
    p.equals(response.updates[0].top, null)
  })
  .then()
  .send('user2', 'Updates', {count: -1}, response => {
    p.equals(response.updates.length, 4)
    p.equals(response.updates[0].type, map.UpdateType.message)
    p.equals(response.updates[1].type, map.UpdateType.updateMessage)
    p.equals(response.updates[2].type, map.UpdateType.deleteMessages)
    p.equals(response.updates[3].type, map.UpdateType.message)
  })
  .then()
  .send('user1', 'Updates', {count: -1}, response => {
    p.equals(response.updates.length, 5)
    p.equals(response.updates[0].type, map.UpdateType.message)
    p.equals(response.updates[1].type, map.UpdateType.updateMessage)
    p.equals(response.updates[2].type, map.UpdateType.deleteMessages)
    p.equals(response.updates[3].type, map.UpdateType.message)

    p.equals(response.updates[4].type, map.UpdateType.deleteMessages)
    p.equals(response.updates[4].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[4].feedId, '000000000000000000000002')
    p.deepEquals(response.updates[4].ids, [get('messageId2')])
    p.equals(response.updates[4].message, null)
  })
}
