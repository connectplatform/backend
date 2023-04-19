module.exports = function (p) {
  p
  .connect('user1')
	.connect('bot1')
	.then()
	.send('bot1', 'User', {}, (response) => {
		p.equals(response.modelName, 'UserResp')
		p.set('botUserId1', response.user.id)
  })
  //check reply keyboard delivery
  .then()
  .send('bot1', 'BotSendMessage', {
    userId: '000000000000000000000001',
    feedType: map.MessageFeedType.chat,
    feedId: get('botUserId1'),
    replyKeyboardMarkup: {
      buttonRows: [
        {buttons: [
          {text: 'Test button'},
        ]},
      ],
    }
  }, response => {
    p.equals(response.modelName, 'BotSendMessageResp')
  })
  .and()
  .wait('user1', 'ChatUpdate', response => {
    p.equals(response.modelName, 'ChatUpdate')
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, get('botUserId1'))
    p.equals(response.update.replyKeyboardMarkup.buttonRows[0].buttons[0].text, 'Test button')
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, get('botUserId1'))
    p.equals(response.updates[0].replyKeyboardMarkup.buttonRows[0].buttons[0].text, 'Test button')
  })
  //check that reply keyboard remains after further updates
  .then()
  .send('bot1', 'Message', {feedType: map.MessageFeedType.chat, feedId: '000000000000000000000001', message: {payload: 'Message 1'}}, response => {
    p.equals(response.modelName, 'MessageResp')
    p.set('messageId1', response.message.id)
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, get('botUserId1'))
    p.equals(response.updates[0].replyKeyboardMarkup.buttonRows[0].buttons[0].text, 'Test button')
  })
  //check remove reply keyboard
  .then()
  .send('bot1', 'BotSendMessage', {
    userId: '000000000000000000000001',
    feedType: map.MessageFeedType.chat,
    feedId: get('botUserId1'),
    removeKeyboardMarkup: {
      // selective: false
    },
  }, response => {
    p.equals(response.modelName, 'BotSendMessageResp')
  })
  .and()
  .wait('user1', 'ChatUpdate', response => {
    p.equals(response.modelName, 'ChatUpdate')
    p.equals(response.update.feedType, map.MessageFeedType.chat)
    p.equals(response.update.feedId, get('botUserId1'))
    p.equals(!!response.update.replyKeyboardMarkup, false)
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, get('botUserId1'))
    p.equals(!!response.updates[0].replyKeyboardMarkup, false)
  })
  //check set reply keyboard once again
  .then()
  .send('bot1', 'BotSendMessage', {
    userId: '000000000000000000000001',
    feedType: map.MessageFeedType.chat,
    feedId: get('botUserId1'),
    replyKeyboardMarkup: {
      buttonRows: [
        {buttons: [
          {text: 'Test button'},
        ]},
      ],
    }
  }, response => {
    p.equals(response.modelName, 'BotSendMessageResp')
  })
  .then()
  .send('user1', 'ChatUpdates', {syncTime: 0}, response => {
    p.equals(response.updates.length, 1)
    p.equals(response.updates[0].feedType, map.MessageFeedType.chat)
    p.equals(response.updates[0].feedId, get('botUserId1'))
    p.equals(response.updates[0].replyKeyboardMarkup.buttonRows[0].buttons[0].text, 'Test button')
  })
};
