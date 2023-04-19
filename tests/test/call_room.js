module.exports = function(p){
  p
  .connect('user1')
  .connect('user2')
  .connect('user3')
  //check group call "[user1] init call -> [user2] accepted -> [user2] ended -> [user2] accepted -> [user2] ended -> [user1] ended"
  //> user1 creates room
  .then()
  .send('user1', 'Room', {room: {topic: 'Room 1', members: ['000000000000000000000002', '000000000000000000000003']}}, response => {
    p.set('roomId', response.room.id)
    p.equals(response.modelName, 'RoomResp')
  })
  //> user1 initiates call
  .then()
  .send('user1', 'InitCall', {feedType: map.MessageFeedType.room, feedId: get('roomId'), video: true}, response => {
    p.equals(response.modelName, 'InitCallResp')
  })
  .and()
  .wait('user2', 'IncomingCall', response => {
    p.equals(!!response.callId, true)
    p.equals(response.feedType, map.MessageFeedType.room)
    p.equals(response.feedId, get('roomId'))
    p.equals(response.initiatorId, '000000000000000000000001')
    p.equals(response.video, true)
  })
  .and()
  .wait('user3', 'IncomingCall', response => {
    p.equals(!!response.callId, true)
    p.equals(response.feedType, map.MessageFeedType.room)
    p.equals(response.feedId, get('roomId'))
    p.equals(response.initiatorId, '000000000000000000000001')
    p.equals(response.video, true)
  })
  .and()
  .wait('user2', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.equals(!!response.update.callId, true)
    p.set('callId', response.update.callId)
  })
  //> user2 checks chat updates
  .then()
  .send('user2', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.room)
    p.equals(response.updates[0].feedId, get('roomId'))
    p.equals(response.updates[0].callId, get('callId'))
  })
  //> user2 accepts call
  .then()
  .send('user2', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.room,
    feedId: get('roomId'),
    status: map.CallStatus.accepted
  }, response => {
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  .and()
  .wait('user1', 'CallStatusChanged', response => {
    p.equals(response.feedType, map.MessageFeedType.room)
    p.equals(response.feedId, get('roomId'))
    p.equals(response.initiatorId, '000000000000000000000002')
    p.equals(response.status, map.CallStatus.accepted)
  })
  .and()
  .wait('user2', 'CallStatusChanged', response => {
    p.equals(response.feedType, map.MessageFeedType.room)
    p.equals(response.feedId, get('roomId'))
    p.equals(response.initiatorId, '000000000000000000000002')
    p.equals(response.status, map.CallStatus.accepted)
  })
  //> user2 hangs
  .then()
  .send('user2', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.room,
    feedId: get('roomId'),
    status: map.CallStatus.ended
  }, response => {
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  //> user2 joins call once more
  .then()
  .send('user2', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.room,
    feedId: get('roomId'),
    status: map.CallStatus.accepted
  }, response => {
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  //> user2 and user3 hangs
  .then()
  .send('user2', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.room,
    feedId: get('roomId'),
    status: map.CallStatus.ended
  }, response => {
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  .then()
  .send('user3', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.room,
    feedId: get('roomId'),
    status: map.CallStatus.ended
  }, response => {
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  //> user1 hangs and it causes call end
  .then()
  .send('user1', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.room,
    feedId: get('roomId'),
    status: map.CallStatus.ended
  }, response => {
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  .and()
  .wait('user1', 'CallStatusChanged', response => {
    p.equals(response.feedType, map.MessageFeedType.room)
    p.equals(response.feedId, get('roomId'))
    p.equals(response.status, map.CallStatus.ended)
    p.equals(response.initiatorId, '000000000000000000000001')
  })
  .and()
  .wait('user1', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.equals(response.update.callId, null)
  })
}
