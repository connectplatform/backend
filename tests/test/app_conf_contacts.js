const AppConfHelper = require('./helpers/app_conf.helper');

module.exports = function (p) {
	const helper = AppConfHelper();
	const testTimeout = 2000;
	const json = {
		"bots": [
			{
				id: null,
				name: 'Bot in contacts',
				username: 'bot_in_contacts',
				descr: 'This bot will added to contacts after registration',
				webhookUrl: 'http://bot.com:3000',
				accessToken: 'botToken',
				photo: 'https://pictures.com/1.jpg',
				thumb: 'https://pictures.com/1.jpg'
			}
		],
		"contacts": {
			"add": [
				{
					"operator": "and",
					"conditions": [
						{"field": "isBot", "values": [{"value": true}]},
						{"field": "username", "values": [{"value": "bot_in_contacts"}]}
					]
				}
			],
			"labels": {
				"isBot": {
					"type": "boolean",
					"values": [
						{
							"value": true,
							"label": "bot"
						}
					]
				}
			}
		}
	};
	
	p
		.then(() => helper.updateFile(json))
		.timeout(testTimeout)
		.then()
		.sendHttp(null, 'RequestVerification', {phone: '+380930603011'}, response => {
			p.equals(response.modelName, 'RequestVerificationResp')
		})
		.then()
		.sendHttp(null, 'ConfirmVerification', {phone: '+380930603011', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
			p.equals(!!response.token, true);
			p.set('token', response.token)
		})
		.then()
		.connect(get('token'))
		.then()
		.send(get('token'), 'UpdateUser', {user: {name: 'John'}}, response => {
			p.equals(response.user.name, 'John');
		})
		.then()
		.send(get('token'), 'SyncContacts', {syncTime: 0}, (response) => {
			p.equals(response.modelName, 'SyncContactsResp');
			p.equals(response.contacts.length, 1);
			const botContact = response.contacts.find(contact => contact.username === json.bots[0].username) || {};
			p.equals(botContact.name, json.bots[0].name);
			p.equals(botContact.thumbnail, json.bots[0].thumb);
			p.equals(botContact.photo, json.bots[0].photo);
			p.equals(botContact.isBot, true);
			p.equals(botContact.friend, true);
			p.equals(botContact.labels.length, 1);
			p.equals(botContact.labels[0], 'bot');
			p.set("botUserId", botContact.userId);
		})
		.then()
		.send(get('token'), 'UpdateContact', {contact: {userId: get('botUserId'), labels: ["custom_1", "custom_2"]}}, (response) => {
			p.equals(response.modelName, 'UpdateContactResp');
			p.equals(response.contact.labels.length, 3);
			p.equals(!!response.contact.labels.find(label => label === 'bot'), true);
			p.equals(!!response.contact.labels.find(label => label === 'custom_1'), true);
			p.equals(!!response.contact.labels.find(label => label === 'custom_2'), true);
		})
		.then()
		.send(get('token'), 'SyncContacts', {syncTime: 0}, (response) => {
			p.equals(response.modelName, 'SyncContactsResp');
			p.equals(response.contacts.length, 1);
			const botContact = response.contacts.find(contact => contact.username === json.bots[0].username) || {};
			p.equals(botContact.labels.length, 3);
			p.equals(!!botContact.labels.find(label => label === 'bot'), true);
			p.equals(!!botContact.labels.find(label => label === 'custom_1'), true);
			p.equals(!!botContact.labels.find(label => label === 'custom_2'), true);
		})
		.then()
		.send(get('token'), 'UpdateContact', {contact: {userId: get('botUserId'), labels: ["custom_3"]}}, (response) => {
			p.equals(response.modelName, 'UpdateContactResp');
			p.equals(response.contact.labels.length, 2);
			p.equals(!!response.contact.labels.find(label => label === 'bot'), true);
			p.equals(!!response.contact.labels.find(label => label === 'custom_3'), true);
		})
		.then()
		.send(get('token'), 'SyncContacts', {syncTime: 0}, (response) => {
			p.equals(response.modelName, 'SyncContactsResp');
			p.equals(response.contacts.length, 1);
			const botContact = response.contacts.find(contact => contact.username === json.bots[0].username) || {};
			p.equals(botContact.labels.length, 2);
			p.equals(!!botContact.labels.find(label => label === 'bot'), true);
			p.equals(!!botContact.labels.find(label => label === 'custom_3'), true);
		})
};
