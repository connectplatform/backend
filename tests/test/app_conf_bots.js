const AppConfHelper = require('./helpers/app_conf.helper');

module.exports = function (p) {
	const helper = AppConfHelper();
	const testTimeout = 2000;
	const bot1 = {
		id: null,
		name: 'Bot 1',
		username: 'bot_1',
		descr: 'test bot #1',
		webhookUrl: 'http://localhost:2301',
		accessToken: 'bot1Token',
		photo: 'https://pictures.com/1.jpg',
		thumb: 'https://pictures.com/1.jpg'
	};

	const bot2 = {
		id: null,
		name: 'Bot 2',
		username: 'bot_2',
		descr: 'test bot #2',
		webhookUrl: 'http://localhost:2302',
		accessToken: 'bot2Token',
		photo: 'https://pictures.com/2.jpg',
		thumb: 'https://pictures.com/2.jpg'
	};

	p
	.then(() => helper.updateFile({bots: [bot1]})) // create bot#1
	.timeout(testTimeout) // wait few seconds when bot user created from config file
	.then()
	.connect(bot1.accessToken)
	.then()
	.send(bot1.accessToken, 'User', {}, (response) => {
		p.equals(response.modelName, 'UserResp');
		p.equals(response.user.name, bot1.name);
		p.equals(response.user.username, bot1.username);
		p.equals(response.user.photo, bot1.photo);
		p.equals(response.user.thumbnail, bot1.thumb);
		p.equals(response.user.isBot, true);
		bot1.id = response.user.id;
		p.set('bot1.id', response.user.id);
	})
	.then(() => {
		bot1.name = 'Bot 1 (updated)';
		helper.updateFile({bots: [bot1, bot2]})
	}) // update bot#1 and create bot2
	.timeout(testTimeout) // wait few seconds when bot user created from config file
	.then()
	.connect(bot2.accessToken)
	.then()
	.send(bot2.accessToken, 'User', {}, (response) => {
		p.equals(response.modelName, 'UserResp');
		p.equals(response.user.name, bot2.name);
		p.equals(response.user.username, bot2.username);
		p.equals(response.user.photo, bot2.photo);
		p.equals(response.user.thumbnail, bot2.thumb);
		p.equals(response.user.isBot, true);
		bot2.id = response.user.id;
		p.set('bot2.id', response.user.id);
	})
	.then()
	.send(bot1.accessToken, 'User', {}, (response) => {
		p.equals(response.modelName, 'UserResp');
		p.equals(response.user.id, bot1.id);
		p.equals(response.user.name, bot1.name);
		p.equals(response.user.username, bot1.username);
		p.equals(response.user.photo, bot1.photo);
		p.equals(response.user.thumbnail, bot1.thumb);
		p.equals(response.user.isBot, true);
	})
	.then()
	.connect('user1')
	.then()
	.send('user1', 'AddBot', {userId: get('bot1.id')}, (response) => {
		p.equals(response.modelName, 'AddBotResp');
		p.equals(response.bot.userId, p.get('bot1.id'));
	})
	.then()
	.send('user1', 'AddBot', {userId: get('bot2.id')}, (response) => {
		p.equals(response.modelName, 'AddBotResp');
		p.equals(response.bot.userId, p.get('bot2.id'));
	})
	.then()
	.send('user1', 'SyncContacts', {syncTime: 0}, (response) => {
		p.equals(response.modelName, 'SyncContactsResp');
		p.equals(response.contacts.length, 2);
		const contactOfBot1 = response.contacts.find(c => c.userId === bot1.id);
		const contactOfBot2 = response.contacts.find(c => c.userId === bot2.id);
		p.equals(contactOfBot1.userId, bot1.id);
		p.equals(contactOfBot1.name, bot1.name);
		p.equals(contactOfBot1.username, bot1.username);
		p.equals(contactOfBot1.photo, bot1.photo);
		p.equals(contactOfBot1.thumbnail, bot1.thumb);

		p.equals(contactOfBot2.userId, bot2.id);
		p.equals(contactOfBot2.name, bot2.name);
		p.equals(contactOfBot2.username, bot2.username);
		p.equals(contactOfBot2.photo, bot2.photo);
		p.equals(contactOfBot2.thumbnail, bot2.thumb);
	})
};
