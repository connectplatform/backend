module.exports = function(p){
  p
  .connect('user1')
  .send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000002', 'Message 1'), response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId', response.message.id)
  })
  .then()
  .send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000002', 'Message 2'), response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId2', response.message.id)
  })
  .then()
  .send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000002', 'Message 3', get('messageId2')), response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId3', response.message.id)
  })
  .then()
  .send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000002', 'Message 4', get('messageId3')), response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId4', response.message.id)
  })
  .then()
  .send('user1', 'Delete', {
    feedType: map.MessageFeedType.chat,
    feedId: '000000000000000000000002',
    ids: [get('messageId2')]
  }, response => {
    p.equals(response.modelName, 'DeleteResp')
  })
  .then()
  .send('user1', 'Retrieve', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000002', count: -1}, response => {
    p.equals(response.messages.length, 3)

    p.equals(response.messages[0].id, get('messageId'))

    p.equals(response.messages[1].id, get('messageId3'))
    p.equals(response.messages[1].reply, null)

    p.equals(response.messages[2].id, get('messageId4'))
    p.equals(response.messages[2].reply.id, get('messageId3'))
  })
}
