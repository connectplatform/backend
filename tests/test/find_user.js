const UserHelper = require('./helpers/user.helper');

module.exports = function (p) {
	const userHelper = new UserHelper(p);
	const users = [
		{name: 'John Doe', phone: '+380930000011'},
		{name: 'John Doe37', phone: '+380930000012'},
		{name: 'John Paul', phone: '+380930000013'},
		{name: 'Johnathan', phone: '+380930000014'},
		{name: 'Jorge', phone: '+380930000015'},
		{name: 'Nick', phone: '+18007543010'},
		{name: 'SimpleUser', phone: '+735555555555'},

		{name: 'Иван Дорн', phone: '+380630000011'},
		{name: 'Іван Франко', phone: '+380630000012'},
	];
	users.forEach(u => userHelper.createUser(u));

	p.clearElasticSearch = true;

	p
		.then()
		.connect('user1')
		.timeout(1000)

		.then()
		.send('user1', 'FindUser', {name: 'Иван'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 2);
			p.equals(response.contacts[0].name, 'Иван Дорн');
			p.equals(response.contacts[1].name, 'Іван Франко');
		})
		.then()
		.send('user1', 'FindUser', {name: 'Іван'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 2);
			p.equals(response.contacts[0].name, 'Іван Франко');
			p.equals(response.contacts[1].name, 'Иван Дорн');
		})

		// Super admin search

		// by name
		.send('user1', 'FindUser', {name: 'Joh'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 4);
			p.equals(response.contacts.every(item => item.phone !== null), true);
			p.equals(response.contacts.every(item => item.originalPhone !== null), true);

			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe37'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Paul'), true);
			p.equals(!!response.contacts.find(item => item.name === 'Johnathan'), true);
		})
		.then()
		// by phone
		.send('user1', 'FindUser', {name: '+38093000001'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 5);
			p.equals(response.contacts.every(item => item.phone !== null), true);
			p.equals(response.contacts.every(item => item.originalPhone !== null), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe37'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Paul'), true);
			p.equals(!!response.contacts.find(item => item.name === 'Johnathan'), true);
			p.equals(!!response.contacts.find(item => item.name === 'Jorge'), true);
		})
		.then()
		.send('user1', 'FindUser', {name: '+380930000015'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 1);
			p.equals(response.contacts.every(item => item.phone !== null), true);
			p.equals(response.contacts.every(item => item.originalPhone !== null), true);
			p.equals(!!response.contacts.find(item => item.name === 'Jorge'), true);
		})
		.then()
		// by userIds
		.send('user1', 'FindUser', {userIds: [get('user.Jorge.id'), get('user.Nick.id'), get('user.John Doe.id')]}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 3);
			p.equals(response.contacts.every(item => item.phone !== null), true);
			p.equals(response.contacts.every(item => item.originalPhone !== null), true);
			p.equals(!!response.contacts.find(item => item.name === 'Jorge'), true);
			p.equals(!!response.contacts.find(item => item.name === 'Nick'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
		})

		.then()
		// Not Admin user search

		.send(get('user.SimpleUser.token'), 'FindUser', {name: 'Joh'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 4);
			p.equals(response.contacts.every(item => item.phone === null), true);
			p.equals(response.contacts.every(item => item.originalPhone === null), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe37'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Paul'), true);
			p.equals(!!response.contacts.find(item => item.name === 'Johnathan'), true);
		})
		.then()
		// by phone
		.send(get('user.SimpleUser.token'), 'FindUser', {name: '+38093000001'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 0);
		})
		.then()
		// by userIds
		.send(get('user.SimpleUser.token'), 'FindUser', {userIds: [get('user.Jorge.id'), get('user.Nick.id'), get('user.John Doe.id')]}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 3);
			p.equals(response.contacts.every(item => item.phone === null), true);
			p.equals(response.contacts.every(item => item.originalPhone === null), true);
			p.equals(!!response.contacts.find(item => item.name === 'Jorge'), true);
			p.equals(!!response.contacts.find(item => item.name === 'Nick'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
		})
		.then()
		// Test exclude
		// John Doe excludeMe ON
		.send(get('user.John Doe.token'), 'UpdateUser', {user: {name: 'John Doe', phone: '+380930000011', excludeMe: true}}, function (response) {
			p.equals(response.modelName, 'UserResp');
			p.equals(response.user.name, 'John Doe');
			p.equals(response.user.excludeMe, true);
		})
		.then()
		.timeout(1000)
		.then()
		// Admin find John Doe by name
		.send('user1', 'FindUser', {name: 'John Doe'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.every(item => item.phone !== null), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe37'), true);
		})
		.then()
		// Admin find John Doe by phone
		.send('user1', 'FindUser', {name: '+380930000011'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 1);
			p.equals(response.contacts.every(item => item.phone !== null), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
		})
		.then()
		// Admin find John Doe by userId
		.send('user1', 'FindUser', {userIds: [ get('user.John Doe.id')]}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 1);
			p.equals(response.contacts.every(item => item.phone !== null), true);
			p.equals(response.contacts.every(item => item.originalPhone !== null), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
		})
		.then()
		// SimpleUser find John Doe by name
		.send(get('user.SimpleUser.token'), 'FindUser', {name: 'John Doe'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.every(item => item.phone === null), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe37'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), false);
		})
		.then()
		// SimpleUser find John Doe by phone
		.send(get('user.SimpleUser.token'), 'FindUser', {name: '+380930000011'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 0);
		})
		.then()
		// excludeMe OFF
		// John Doe excludeMe OFF
		.send(get('user.John Doe.token'), 'UpdateUser', {user: {name: 'John Doe', phone: '+380930000011', excludeMe: false}}, function (response) {
			p.equals(response.modelName, 'UserResp');
			p.equals(response.user.name, 'John Doe');
			p.equals(response.user.excludeMe, false);
		})
		.then()
		.timeout(1000)
		.then()

		// SimpleUser find John Doe by name
		.send(get('user.SimpleUser.token'), 'FindUser', {name: 'John Doe'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.every(item => item.phone === null), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe37'), true);
		})
		.then()
		// SimpleUser find John Doe by phone
		.send(get('user.SimpleUser.token'), 'FindUser', {name: '+380930000011'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 0);
		})
		.then()
		// SimpleUser find John Doe by userId
		.send(get('user.SimpleUser.token'), 'FindUser', {userIds: [ get('user.John Doe.id')]}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.length, 1);
			p.equals(response.contacts.every(item => item.phone === null), true);
			p.equals(response.contacts.every(item => item.originalPhone === null), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), true);
		})
		.then()
		.send(get('user.John Doe.token'), 'FindUser', {name: 'John Doe'}, response => {
			p.equals(response.modelName, 'FindUserResp');
			p.equals(response.contacts.every(item => item.phone === null), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe37'), true);
			p.equals(!!response.contacts.find(item => item.name === 'John Doe'), false);
		})
};


