module.exports = p => {
  p
  .connect('user1')
  .connect('user2')
  .connect('user3')
  //check create room
  .send('user1', 'Room', {room: {members: ['000000000000000000000002', '000000000000000000000003'], topic: 'Topic 1'}}, response => {
    p.equals(response.room.topic, 'Topic 1')
    p.deepEquals(response.room.members.sort(), ['000000000000000000000001', '000000000000000000000002', '000000000000000000000003'])
    p.deepEquals(response.room.admins.sort(), ['000000000000000000000001'])
  })
  .and()
  .wait('user1', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.deepEquals(response.update.room.members.sort(), ['000000000000000000000001', '000000000000000000000002', '000000000000000000000003'])
    p.set('roomId', response.update.room.id)
  })
  .and()
  .wait('user2', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room)
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room)
  })
  //check kick from room
  .then()
  .send('user1', 'KickFromRoom', {roomId: get('roomId'), members: ['000000000000000000000003']}, response => {
    p.deepEquals(response.room.members.sort(), ['000000000000000000000001', '000000000000000000000002'])
    p.deepEquals(response.room.admins.sort(), ['000000000000000000000001'])
  })
  .and()
  .wait('user2', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.equals(response.update.feedId, get('roomId'))
    p.deepEquals(response.update.room.members.sort(), ['000000000000000000000001', '000000000000000000000002'])
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.equals(true, ['000000000000000000000666', get('roomId')].includes(response.update.feedId))
    p.equals(true, ['system.chat.created', 'system.user.has.been.kicked.from.room'].includes(response.update.top.systemMessageType))
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 2)
    p.equals(response.updates[0].feedType, map.MessageFeedType.room)
    p.equals(response.updates[0].feedId, get('roomId'))
    p.equals(response.updates[0].unread, 0)
    p.equals(response.updates[0].deleted, true)
  })
  //check change topic
  .then()
  .send('user1', 'ChangeRoomTopic', {roomId: get('roomId'), topic: 'Topic 2'}, response => {
    p.equals(response.modelName, 'ChangeRoomTopicResp')
    p.equals(response.room.topic, 'Topic 2')
  })
  .and()
  .wait('user1', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.equals(response.update.feedId, get('roomId'))
    p.equals(response.update.room.topic, 'Topic 2')
  })
  .and()
  .wait('user2', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.equals(response.update.feedId, get('roomId'))
    p.equals(response.update.room.topic, 'Topic 2')
  })
  .then()
  .send('user2', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].room.topic, 'Topic 2')
  })
  //check add user to room
  .then()
  .send('user1', 'AddToRoom', {roomId: get('roomId'), members: ['000000000000000000000003']}, response => {
    p.equals(response.modelName, 'AddToRoomResp')
  })
  .and()
  .wait('user1', 'ChatUpdate', response => {
    p.equals(response.update.feedType, map.MessageFeedType.room)
    p.equals(response.update.feedId, get('roomId'))
    p.deepEquals(response.update.room.members.sort(), ['000000000000000000000001', '000000000000000000000002', '000000000000000000000003'])
  })
  .and()
  .wait('user2', 'ChatUpdate', response => {
    p.deepEquals(response.update.room.members.sort(), ['000000000000000000000001', '000000000000000000000002', '000000000000000000000003'])
  })
  .and()
  .wait('user3', 'ChatUpdate', response => {
    p.deepEquals(response.update.room.members.sort(), ['000000000000000000000001', '000000000000000000000002', '000000000000000000000003'])
  })
  //check quit from room
  .then()
  .send('user3', 'QuitFromRoom', {roomId: get('roomId')}, function(response){
    p.equals(response.modelName, 'QuitFromRoomResp')
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 2)
    p.equals(response.updates[0].feedType, map.MessageFeedType.room)
    p.equals(response.updates[0].feedId, get('roomId'))
    p.equals(response.updates[0].unread, 0)
    p.equals(response.updates[0].deleted, true)
  })
  .then()
  .send('user2', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.deepEquals(response.updates[0].room.members, ['000000000000000000000001', '000000000000000000000002'])
  })
  //check change role
  .then()
  .timeout(1000)
  .then()
  .send('user1', 'ChangeRoleInRoom', {
    roomId: get('roomId'),
    members: [
      {id: '000000000000000000000001', role: map.RoomMemberRole.member},
      {id: '000000000000000000000002', role: map.RoomMemberRole.admin}
    ]
  }, response => {
    p.deepEquals(response.room.members.sort(), ['000000000000000000000001', '000000000000000000000002'])
    p.deepEquals(response.room.admins.sort(), ['000000000000000000000002'])
  })
  .and()
  .wait('user1', 'ChatUpdate', response => {
    p.deepEquals(response.update.room.members.sort(), ['000000000000000000000001', '000000000000000000000002'])
    p.deepEquals(response.update.room.admins.sort(), ['000000000000000000000002'])
  })
  .and()
  .wait('user2', 'ChatUpdate', response => {
    p.deepEquals(response.update.room.members.sort(), ['000000000000000000000001', '000000000000000000000002'])
    p.deepEquals(response.update.room.admins.sort(), ['000000000000000000000002'])
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 2)
    p.equals(response.updates[0].feedType, map.MessageFeedType.room)
    p.equals(response.updates[0].feedId, get('roomId'))
    p.deepEquals(response.updates[0].room.members.sort(), ['000000000000000000000001', '000000000000000000000002'])
    p.deepEquals(response.updates[0].room.admins.sort(), ['000000000000000000000002'])
  })
  //check that you can't delete room with 2 and more members
  .then()
  .send('user2', 'DeleteRoom', {roomId: get('roomId')}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.roomMembersNotKicked);
  })
  .then()
  .send('user2', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.room)
    p.equals(response.updates[0].feedId, get('roomId'))
    p.equals(response.updates[0].deleted, false)
  })
  //check delete room
  .then()
  .send('user2', 'KickFromRoom', {roomId: get('roomId'), members: ['000000000000000000000001']}, response => {
    p.deepEquals(response.room.members.sort(), ['000000000000000000000002'])
    p.deepEquals(response.room.admins.sort(), ['000000000000000000000002'])
  })
  .then()
  .send('user2', 'DeleteRoom', {roomId: get('roomId')}, response => {
    p.equals(response.modelName, 'DeleteRoomResp')
  })
  .then()
  .send('user2', 'ChatUpdates', {}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.room)
    p.equals(response.updates[0].feedId, get('roomId'))
    p.equals(response.updates[0].unread, 0)
    p.equals(response.updates[0].deleted, true)
  })
}
