module.exports = function(p){
  p
  .connect('user1')
  .connect('user2')
  .connect('user3')
  //chat
  .send('user1', 'LiberateSynapse', {
    synapse: {
      feedType: map.MessageFeedType.chat,
      feedId: '000000000000000000000002',
      data: [],
      metadata: '{"foo": "bar"}'
    }
  }, response => {
    p.equals(response.modelName, 'LiberateSynapseResp')
  })
  .and()
  .wait('user2', 'SynapseDiscovered', response => {
    p.equals(response.synapse.feedType, map.MessageFeedType.chat)
    p.equals(response.synapse.feedId, '000000000000000000000001')
    p.equals(response.synapse.sender, '000000000000000000000001')
    p.equals(response.synapse.metadata, '{"foo": "bar"}')
  })
  //room
  .then()
  .send('user1', 'Room', {room: {topic: 'Room 1', members: ['000000000000000000000002', '000000000000000000000003']}}, response => {
    p.set('roomId', response.room.id)
    p.equals(response.modelName, 'RoomResp')
  })
  .then()
  .send('user1', 'LiberateSynapse', {
    synapse: {
      feedType: map.MessageFeedType.room,
      feedId: get('roomId'),
      data: [],
      metadata: '{"foo": "bar"}'
    },
    sendToMe: true
  }, response => {
    p.equals(response.modelName, 'LiberateSynapseResp')
  })
  .and()
  .wait('user1', 'SynapseDiscovered', response => {
    p.equals(response.synapse.feedType, map.MessageFeedType.room)
    p.equals(response.synapse.feedId, get('roomId'))
    p.equals(response.synapse.sender, '000000000000000000000001')
    p.equals(response.synapse.metadata, '{"foo": "bar"}')
  })
  .and()
  .wait('user2', 'SynapseDiscovered', response => {
    p.equals(response.synapse.feedType, map.MessageFeedType.room)
    p.equals(response.synapse.feedId, get('roomId'))
    p.equals(response.synapse.sender, '000000000000000000000001')
    p.equals(response.synapse.metadata, '{"foo": "bar"}')
  })
  .and()
  .wait('user3', 'SynapseDiscovered', response => {
    p.equals(response.synapse.feedType, map.MessageFeedType.room)
    p.equals(response.synapse.feedId, get('roomId'))
    p.equals(response.synapse.sender, '000000000000000000000001')
    p.equals(response.synapse.metadata, '{"foo": "bar"}')
  })
}
