module.exports = function (p) {
  p
    .connect('user1')
    .connect('user2')
    .send('user1', 'UpdateUser', {user: {
      name: 'User1',
      photo: 'http://user1.photo',
      thumbnail: 'http://user1.thumbnail',
      bio: 'I Am User1',
      sendBioToNewContacts: true
    }}, response => {
      p.equals(response.modelName, 'UserResp')
      p.equals(response.user.sendBioToNewContacts, true)
    })
    .then()
    .send('user2', 'AddContact', {contact: {userId: '000000000000000000000001'}}, response => {
      p.equals(response.modelName, 'AddContactResp')
    })
    .then()
    .send('user2', 'Retrieve', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000001', count: -1}, response => {
      p.equals(response.messages.length, 2)
      p.equals(response.messages[0].systemMessageType, 'system.you.added.user.to.contacts')
      p.equals(response.messages[1].payload, 'I Am User1')
    })
}
