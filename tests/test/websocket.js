module.exports = function(p){
  p
  .connect('user1')
  .connect('user2')
  .then()
  .send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, '000000000000000000000002', 'How r u?'), response => {
    p.equals(response.modelName, 'MessageResp');
    p.set('messageId1', response.message.id)
  })
  .wait('user2', 'Update', () => {
    p.pass()
  })
  .then()
  .send('user1', 'Retrieve', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000002', count: -1}, response => {
    p.equals(response.messages.length, 1);
    p.equals(response.messages[0].id, get('messageId1'))
  })
};
