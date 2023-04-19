module.exports = function(p){
  p
  .connect('user1')
  .connect('user2')
  //check private call "[user2] init call -> [user1] accepted -> [user2] ended"
  //> user2 initiates call
  .send('user2', 'InitCall', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000001', video: true}, response => {
    p.equals(response.modelName, 'InitCallResp')
    p.set('callId', response.callId)
  })
  .and()
  .wait('user1', 'IncomingCall', response => {
    p.set('incomingCallId', response.callId)
    p.equals(response.feedType, map.MessageFeedType.chat)
    p.equals(response.feedId, '000000000000000000000002')
    p.equals(response.initiatorId, '000000000000000000000002')
    p.equals(response.video, true)
  })
  //> user1 accepts call
  .then()
  .send('user1', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.chat,
    feedId: '000000000000000000000002',
    status: map.CallStatus.accepted
  }, response => {
    p.equals(get('callId'), get('incomingCallId'))
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  .and()
  .wait('user1', 'CallStatusChanged', response => {
    p.equals(response.feedType, map.MessageFeedType.chat)
    p.equals(response.feedId, '000000000000000000000002')
    p.equals(response.initiatorId, '000000000000000000000001')
    p.equals(response.status, map.CallStatus.accepted)
  })
  .and()
  .wait('user2', 'CallStatusChanged', response => {
    p.equals(response.feedType, map.MessageFeedType.chat)
    p.equals(response.feedId, '000000000000000000000001')
    p.equals(response.initiatorId, '000000000000000000000001')
    p.equals(response.status, map.CallStatus.accepted)
  })
  //> user2 ends call
  .then()
  .send('user2', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.chat,
    feedId: '000000000000000000000001',
    status: map.CallStatus.ended
  }, response => {
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  .and()
  .wait('user1', 'CallStatusChanged', response => {
    p.equals(response.feedType, map.MessageFeedType.chat)
    p.equals(response.feedId, '000000000000000000000002')
    p.equals(response.initiatorId, '000000000000000000000002')
    p.equals(response.status, map.CallStatus.ended)
  })
  .and()
  .wait('user2', 'CallStatusChanged', response => {
    p.equals(response.feedType, map.MessageFeedType.chat)
    p.equals(response.feedId, '000000000000000000000001')
    p.equals(response.initiatorId, '000000000000000000000002')
    p.equals(response.status, map.CallStatus.ended)
  })
  .and()
  .wait('user2', 'Update', response => {
    p.equals(response.update.type, map.UpdateType.message)
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, '000000000000000000000001')
    p.equals(response.update.message.call.id, get('callId'))
  })
  //> check that you cant change status of ended call
  .then()
  .send('user2', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.chat,
    feedId: '000000000000000000000001',
    status: map.CallStatus.accepted
  }, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.notFound)
  })
  //check private call "[user2] init call -> [user1] reject"
  //> user2 initiates call
  .then()
  .send('user2', 'InitCall', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000001'}, response => {
    p.equals(response.modelName, 'InitCallResp')
  })
  .and()
  .wait('user1', 'IncomingCall', response => {
    p.equals(!!response.callId, true)
    p.equals(response.feedType, map.MessageFeedType.chat)
    p.equals(response.feedId, '000000000000000000000002')
    p.equals(response.initiatorId, '000000000000000000000002')
  })
  //> user1 rejects call
  .then()
  .send('user1', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.chat,
    feedId: '000000000000000000000002',
    status: map.CallStatus.rejected
  }, response => {
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  .and()
  .wait('user1', 'CallStatusChanged', response => {
    p.equals(response.feedType, map.MessageFeedType.chat)
    p.equals(response.feedId, '000000000000000000000002')
    p.equals(response.initiatorId, '000000000000000000000001')
    p.equals(response.status, map.CallStatus.rejected)
  })
  .and()
  .wait('user2', 'CallStatusChanged', response => {
    p.equals(response.feedType, map.MessageFeedType.chat)
    p.equals(response.feedId, '000000000000000000000001')
    p.equals(response.initiatorId, '000000000000000000000001')
    p.equals(response.status, map.CallStatus.rejected)
  })
  //> check that you cant change status of ended call
  .then()
  .send('user1', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.chat,
    feedId: '000000000000000000000002',
    status: map.CallStatus.accepted
  }, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.notFound)
  })
  //user1 calls user2
  .then()
  .send('user1', 'InitCall', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000002'}, response => {
    p.equals(response.modelName, 'InitCallResp')
  })
  .then()
  .send('user2', 'UpdateCallStatus', {
    feedType: map.MessageFeedType.chat,
    feedId: '000000000000000000000001',
    status: map.CallStatus.rejected
  }, response => {
    p.equals(response.modelName, 'UpdateCallStatusResp')
  })
  .and()
  .wait('user1', 'ChatUpdate', response => {
    p.equals(response.update.top.kind, map.MessageKind.call)
    p.equals(response.update.top.call.initiatorId, '000000000000000000000001')
  })
  //check get calls for user1
  .then()
  .send('user1', 'Calls', {count: -1}, response => {
    p.equals(response.calls.length, 3)

    p.equals(response.calls[0].feedType, map.MessageFeedType.chat)
    p.equals(response.calls[0].feedId, '000000000000000000000002')
    p.equals(response.calls[0].initiatorId, '000000000000000000000002')
    p.equals(response.calls[0].status, map.CallStatus.ended)
    p.equals(response.calls[0].direction, map.CallDirection.inbound)

    p.equals(response.calls[1].feedType, map.MessageFeedType.chat)
    p.equals(response.calls[1].feedId, '000000000000000000000002')
    p.equals(response.calls[1].initiatorId, '000000000000000000000002')
    p.equals(response.calls[1].status, map.CallStatus.rejected)
    p.equals(response.calls[1].direction, map.CallDirection.inbound)

    p.equals(response.calls[2].feedType, map.MessageFeedType.chat)
    p.equals(response.calls[2].feedId, '000000000000000000000002')
    p.equals(response.calls[2].initiatorId, '000000000000000000000001')
    p.equals(response.calls[2].status, map.CallStatus.rejected)
    p.equals(response.calls[2].direction, map.CallDirection.outbound)
  })
  //check get calls for user2
  .then()
  .send('user2', 'Calls', {count: -1}, response => {
    p.equals(response.calls.length, 3)

    p.set('callId3', response.calls[0].id)
    p.set('callId2', response.calls[1].id)
    p.set('callId1', response.calls[2].id)

    p.equals(response.calls[0].feedType, map.MessageFeedType.chat)
    p.equals(response.calls[0].feedId, '000000000000000000000001')
    p.equals(response.calls[0].initiatorId, '000000000000000000000002')
    p.equals(response.calls[0].status, map.CallStatus.ended)
    p.equals(response.calls[0].direction, map.CallDirection.outbound)

    p.equals(response.calls[1].feedType, map.MessageFeedType.chat)
    p.equals(response.calls[1].feedId, '000000000000000000000001')
    p.equals(response.calls[1].initiatorId, '000000000000000000000002')
    p.equals(response.calls[1].status, map.CallStatus.rejected)
    p.equals(response.calls[1].direction, map.CallDirection.outbound)

    p.equals(response.calls[2].feedType, map.MessageFeedType.chat)
    p.equals(response.calls[2].feedId, '000000000000000000000001')
    p.equals(response.calls[2].initiatorId, '000000000000000000000001')
    p.equals(response.calls[2].status, map.CallStatus.rejected)
    p.equals(response.calls[2].direction, map.CallDirection.inbound)
  })
  //check get calls with paging
  //>check stop
  .then()
  .send('user2', 'Calls', {stop: get('callId2'), count: -1}, response => {
    p.equals(response.calls[0].id, get('callId1'))
    p.equals(response.calls[0].feedType, map.MessageFeedType.chat)
    p.equals(response.calls[0].feedId, '000000000000000000000001')
    p.equals(response.calls[0].initiatorId, '000000000000000000000001')
    p.equals(response.calls[0].status, map.CallStatus.rejected)
    p.equals(response.calls[0].direction, map.CallDirection.inbound)
  })
  //>check top
  .then()
  .send('user2', 'Calls', {top: get('callId2'), count: -1}, response => {
    p.equals(response.calls[0].id, get('callId3'))
    p.equals(response.calls[0].feedType, map.MessageFeedType.chat)
    p.equals(response.calls[0].feedId, '000000000000000000000001')
    p.equals(response.calls[0].initiatorId, '000000000000000000000002')
    p.equals(response.calls[0].status, map.CallStatus.ended)
    p.equals(response.calls[0].direction, map.CallDirection.outbound)
  })
}
