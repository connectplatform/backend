module.exports = function(p){
  p
  .connect('user1')
  // check get all bots
  .then()
  .send('user1', 'Bots', {}, response => {
    p.equals(response.bots.length, 1);
    p.equals(response.bots[0].username, 'testbot');
    p.equals(response.bots[0].webviewUrlMain, 'http://localhost:5000/bot/testbot/webview/main');
    p.equals(response.bots[0].webviewUrlChat, 'http://localhost:5000/bot/testbot/webview/chat');
    p.set('botUserId1', response.bots[0].userId);
  })
  // check get single bot
  .then()
  .send('user1', 'Bot', {userId: get('botUserId1')}, response => {
    p.equals(response.modelName, 'BotResp');
    p.equals(response.bot.userId, get('botUserId1'));
    p.equals(response.bot.username, 'testbot');
    p.equals(response.bot.webviewUrlMain, 'http://localhost:5000/bot/testbot/webview/main');
    p.equals(response.bot.webviewUrlChat, 'http://localhost:5000/bot/testbot/webview/chat');
  })
  // check exec as user
  .then()
  .send('user1', 'ExecAsUser', {
  	userId: '000000000000000000000003',
	  request: encode({modelName: 'User'})
  }, response => {
    p.equals(response.modelName, 'ExecAsUserResp');
    const responseMessage = decode(response.response);
    p.equals(responseMessage.user.id, '000000000000000000000003');
    p.equals(responseMessage.user.name, '3')
  })
};

function encode(data) {
	return toBuffer(transponder.hydrator.encode(data))
}

function decode(data) {
	return transponder.hydrator.decode(toArrayBuffer(data));
}

function toBuffer(ab) {
	var buffer = Buffer.alloc(ab.byteLength);
	var view = new Uint8Array(ab);
	for (var i = 0; i < buffer.length; ++i){
		buffer[i] = view[i];
	}
	return buffer;
}

function toArrayBuffer(buffer) {
	var ab = new ArrayBuffer(buffer.length);
	var view = new Uint8Array(ab);
	for (var i = 0; i < buffer.length; ++i) {
		view[i] = buffer[i];
	}
	return ab;
}
