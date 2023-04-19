module.exports = function(p){
  p
  .connect('user1')
  //get new circle
  .send('user1', 'GetCircle', {supervisor: 'testbot', params: '*action=buyCar&*carId=22&topic=TheTopic1&picture=pic.jpg&thumbnail=thumb.jpg', members: ['000000000000000000000002']}, response => {
    p.equals(response.modelName, 'GetCircleResp')
    p.set('roomId1', response.roomId)
  })
  .and()
  .wait('user1', 'ChatUpdate', response => {
    p.equals(response.update.top.systemMessageType, 'system.chat.created')
    p.equals(response.update.room.circle.supervisor, 'testbot')
    p.equals(response.update.room.circle.params, 'action=buyCar&carId=22&picture=pic.jpg&thumbnail=thumb.jpg&topic=TheTopic1')
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.room)
    p.equals(response.updates[0].feedId, get('roomId1'))
    p.equals(response.updates[0].room.members.length, 3)
    p.equals(response.updates[0].room.members[0], '000000000000000000000001')
    p.equals(response.updates[0].room.members[1], '5d5981b176be8222a0c623d3')
    p.equals(response.updates[0].room.members[2], '000000000000000000000002')
    p.equals(response.updates[0].room.topic, 'TheTopic1')
    p.equals(response.updates[0].room.picture, 'pic.jpg')
    p.equals(response.updates[0].room.thumbnail, 'thumb.jpg')
    p.equals(response.updates[0].room.circle.supervisor, 'testbot')
    p.equals(response.updates[0].room.circle.params, 'action=buyCar&carId=22&picture=pic.jpg&thumbnail=thumb.jpg&topic=TheTopic1')
  })
  //get circle with the same parameters must return same circle
  .then()
  .send('user1', 'GetCircle', {supervisor: 'testbot', params: '*action=buyCar&*carId=22', members: ['000000000000000000000005']}, response => {
    p.equals(response.modelName, 'GetCircleResp')
    p.equals(response.roomId, get('roomId1'))
    p.equals(response.update.room.members.length, 4)
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.equals(response.update.feedId, get('roomId1'))
    p.equals(response.update.top.systemMessageType, 'system.chat.created')
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
  })
  //get circle with different parameters must return new circle
  .then()
  .send('user1', 'GetCircle', {supervisor: 'testbot', params: '*action=buyCar&*carId=33', members: ['000000000000000000000005']}, response => {
    p.equals(response.modelName, 'GetCircleResp')
    p.equals(response.update.room.members.length, 3)
    p.set('roomId2', response.roomId)
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 2)
    p.equals(response.updates[0].feedId, get('roomId1'))
    p.equals(response.updates[0].room.topic, 'TheTopic1')
    p.equals(response.updates[0].room.circle.supervisor, 'testbot')
    p.equals(response.updates[0].room.circle.params, 'action=buyCar&carId=22&picture=pic.jpg&thumbnail=thumb.jpg&topic=TheTopic1')

    p.equals(response.updates[1].feedId, get('roomId2'))
    p.equals(response.updates[1].room.topic, 'New group')
    p.equals(response.updates[1].room.circle.supervisor, 'testbot')
    p.equals(response.updates[1].room.circle.params, 'action=buyCar&carId=33')
  })
  //get circle with the different non required parameter must return same circle
  .then()
  .send('user1', 'GetCircle', {supervisor: 'testbot', params: '*action=buyCar&*carId=33&topic=TheTopic2&picture=pic2.jpg&thumbnail=thumb2.jpg&foo=bar', members: ['000000000000000000000005']}, response => {
    p.equals(response.modelName, 'GetCircleResp')
    p.equals(response.roomId, get('roomId2'))
    p.equals(response.update.room.members.length, 4)
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.equals(response.update.feedId, get('roomId2'))
    p.equals(response.update.room.topic, 'TheTopic2')
    p.equals(response.update.room.picture, 'pic2.jpg')
    p.equals(response.update.room.thumbnail, 'thumb2.jpg')
    p.equals(response.update.room.circle.params, 'action=buyCar&carId=33&foo=bar&picture=pic2.jpg&thumbnail=thumb2.jpg&topic=TheTopic2')
    p.equals(response.update.top.systemMessageType, 'system.chat.created')
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 2)
    p.equals(response.updates[1].room.topic, 'TheTopic2')
  })
  //check optional parameters persistence
  .then()
  .connect('user3')
  .then()
  .send('user3', 'GetCircle', {supervisor: 'testbot', params: 'action=first&*carId=1'}, response => {
    p.equals(response.modelName, 'GetCircleResp')
    p.equals(response.update.room.members.length, 2)
    p.set('roomId2', response.roomId)
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedId, get('roomId2'))
    p.equals(response.updates[0].room.circle.supervisor, 'testbot')
    p.equals(response.updates[0].room.circle.params, 'action=first&carId=1')
  })
  .then()
  .send('user3', 'GetCircle', {supervisor: 'testbot', params: 'action=second&*carId=1'}, response => {
    p.equals(response.modelName, 'GetCircleResp')
    p.equals(response.update.room.members.length, 2)
    p.set('roomId2', response.roomId)
  })
  .then()
  .send('user3', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedId, get('roomId2'))
    p.equals(response.updates[0].room.circle.supervisor, 'testbot')
    p.equals(response.updates[0].room.circle.params, 'action=second&carId=1')
  })
}
