module.exports = function(p){
	const directionOldest = map.RetrieveDirection.up;
	const directionNewest = map.RetrieveDirection.down;
	const userId1 = '000000000000000000000001';
	const userId2 = '000000000000000000000002';
  p
  .connect('user1')
  .connect('user2')
  .then()
  .send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '1'), response => p.set('messageId1', response.message.id))
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '2'), response => p.set('messageId2', response.message.id))
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '3'), response => p.set('messageId3', response.message.id))
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '4'), response => p.set('messageId4', response.message.id))
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '5'), response => p.set('messageId5', response.message.id))
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '6'), response => p.set('messageId6', response.message.id))
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '7'), response => p.set('messageId7', response.message.id))
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '8'), response => p.set('messageId8', response.message.id))
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '9'), response => p.set('messageId9', response.message.id))
	.then()
	.send('user1', 'Message', global.entityHelper.generateMessage(map.MessageFeedType.chat, userId2, '10'), response => p.set('messageId10', response.message.id))
	.then()
	// Top - message#5 and Direction - down to get message#6 and message#7
  .send('user1', 'Retrieve', {feedType: map.MessageFeedType.chat, feedId: userId2, top: get('messageId5'), count: 2, direction: directionNewest}, response => {
  	p.equals(response.modelName, 'RetrieveResp');
  	p.equals(response.messages.length, 2);
  	p.equals(response.messages[0].id, p.get('messageId6'));
  	p.equals(response.messages[1].id, p.get('messageId7'));
  })
	// Top - message#5 and Direction - up to get message#4 and message#3
	.then()
	.send('user1', 'Retrieve', {feedType: map.MessageFeedType.chat, feedId: userId2, top: get('messageId5'), count: 2, direction: directionOldest}, response => {
		p.equals(response.modelName, 'RetrieveResp');
		p.equals(response.messages.length, 2);
		p.equals(response.messages[0].id, p.get('messageId3'));
		p.equals(response.messages[1].id, p.get('messageId4'));
	})
	// Top - unknown and Direction - down to get first messages
	.then()
	.send('user1', 'Retrieve', {feedType: map.MessageFeedType.chat, feedId: userId2, count: 2, direction: directionNewest}, response => {
		p.equals(response.modelName, 'RetrieveResp');
		p.equals(response.messages.length, 2);
		p.equals(response.messages[0].id, p.get('messageId1'));
		p.equals(response.messages[1].id, p.get('messageId2'));
	})
	// Top - unknown and Direction - up to get last messages
	.then()
	.send('user1', 'Retrieve', {feedType: map.MessageFeedType.chat, feedId: userId2, count: 2, direction: directionOldest}, response => {
		p.equals(response.modelName, 'RetrieveResp');
		p.equals(response.messages.length, 2);
		p.equals(response.messages[0].id, p.get('messageId9'));
		p.equals(response.messages[1].id, p.get('messageId10'));
	})
	.then()
	.send('user1', 'Retrieve', {feedType: map.MessageFeedType.chat, feedId: userId2, count: -1, direction: directionNewest}, response => {
	 p.equals(response.modelName, 'RetrieveResp');
	  p.equals(response.messages.length, 10);
		p.equals(response.messages[0].id, p.get('messageId1'));
		p.equals(response.messages[1].id, p.get('messageId2'));
		p.equals(response.messages[2].id, p.get('messageId3'));
		p.equals(response.messages[3].id, p.get('messageId4'));
		p.equals(response.messages[4].id, p.get('messageId5'));
		p.equals(response.messages[5].id, p.get('messageId6'));
		p.equals(response.messages[6].id, p.get('messageId7'));
		p.equals(response.messages[7].id, p.get('messageId8'));
		p.equals(response.messages[8].id, p.get('messageId9'));
		p.equals(response.messages[9].id, p.get('messageId10'));
	})
	.send('user2', 'Retrieve', {feedType: map.MessageFeedType.chat, feedId: userId1, count: -1, direction: directionOldest}, response => {
	  p.equals(response.modelName, 'RetrieveResp');
	  p.equals(response.messages.length, 10);
	 p.equals(response.messages[0].id, p.get('messageId1'));
	 p.equals(response.messages[1].id, p.get('messageId2'));
	 p.equals(response.messages[2].id, p.get('messageId3'));
	 p.equals(response.messages[3].id, p.get('messageId4'));
	 p.equals(response.messages[4].id, p.get('messageId5'));
	 p.equals(response.messages[5].id, p.get('messageId6'));
	 p.equals(response.messages[6].id, p.get('messageId7'));
	 p.equals(response.messages[7].id, p.get('messageId8'));
	 p.equals(response.messages[8].id, p.get('messageId9'));
	 p.equals(response.messages[9].id, p.get('messageId10'));
	})
};
