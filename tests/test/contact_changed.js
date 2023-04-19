module.exports = function (p) {
	const userId1 = '000000000000000000000001';
	const userId2 = '000000000000000000000002';
	const userId3 = '000000000000000000000003';

	const imDirectoryIsTurnedOn = false; // im_directory:is_turnedon()

	/*
	 Contacts scheme:
		 user1 : [user2, user3]
		 user2 : [user1]
		 user3 : []
	*/

	p
		.connect('user1')
		.connect('user2')
		.connect('user3')

		.then()

		.send('user1', 'UpdateUser', {user: {
				name: 'User1',
				photo: 'http://user1.photo',
				thumbnail: 'http://user1.thumbnail'
			}}, (response) => {
			p.equals(response.modelName, 'UserResp');
			p.equals(response.user.name, 'User1');
			p.equals(response.user.photo, 'http://user1.photo');
			p.equals(response.user.thumbnail, 'http://user1.thumbnail');
		})
		.then()
		.send('user2', 'UpdateUser', {user: {
				name: 'User2',
				photo: 'http://user2.photo',
				thumbnail: 'http://user2.thumbnail'
			}}, (response) => {
			p.equals(response.modelName, 'UserResp');
			p.equals(response.user.name, 'User2');
			p.equals(response.user.photo, 'http://user2.photo');
			p.equals(response.user.thumbnail, 'http://user2.thumbnail');
		})
		.then()
		.send('user3', 'UpdateUser', {user: {
				name: 'User3',
				photo: 'http://user3.photo',
				thumbnail: 'http://user3.thumbnail'
			}}, (response) => {
			p.equals(response.modelName, 'UserResp');
			p.equals(response.user.name, 'User3');
			p.equals(response.user.photo, 'http://user3.photo');
			p.equals(response.user.thumbnail, 'http://user3.thumbnail');
		})

		.then() // user2 add to contact user1

		.send('user2', 'AddContact', {contact: {userId: userId1}}, (response) => {
			p.equals(response.modelName, 'AddContactResp');
			p.equals(response.contact.userId, userId1);
			p.equals(response.contact.status, map.ContactStatus.friend);
			p.equals(response.contact.name, 'User1');
			p.equals(response.contact.photo, 'http://user1.photo');
			p.equals(response.contact.thumbnail, 'http://user1.thumbnail');
		})
		.and()
		.wait('user2', 'ContactChanged', (response) => {
			p.equals(response.contact.userId, userId1);
			p.equals(response.contact.status, map.ContactStatus.friend);
			p.equals(response.contact.name, 'User1');
			p.equals(response.contact.photo, 'http://user1.photo');
			p.equals(response.contact.thumbnail, 'http://user1.thumbnail');
		})
		.and()
		.wait('user1', 'ContactChanged', (response) => {
			p.equals(response.contact.userId, userId2);
			p.equals(response.contact.status, map.ContactStatus.pending);
			p.equals(response.contact.name, 'User2');
			p.equals(response.contact.photo, 'http://user2.photo');
			p.equals(response.contact.thumbnail, 'http://user2.thumbnail');
		})
		.and()
		// user2 get Update because hi get system message that user1 was added to contact
		.wait('user2', 'Update', (response) => {
			p.equals(response.update.message.type, map.MessageType.systemNotification);
			p.equals(response.update.feedId, userId1);
		})
		.and()
		// user2 get ChatUpdate because hi get system message that user1 was added to contact
		.wait('user2', 'ChatUpdate', (response) => {
			p.equals(response.update.top.type, map.MessageType.systemNotification);
			p.equals(response.update.feedId, userId1);
			p.equals(response.update.name, 'User1');
			p.equals(response.update.thumbnail, 'http://user1.thumbnail');
			p.equals(response.update.top.systemMessageType, 'system.you.added.user.to.contacts');
		})

		.then() // user1 add to contact user2

		.send('user1', 'AddContact', {contact: {userId: userId2}}, (response) => {
			p.equals(response.modelName, 'AddContactResp');
			p.equals(response.contact.userId, userId2);
			p.equals(response.contact.status, map.ContactStatus.friend);
			p.equals(response.contact.name, 'User2');
			p.equals(response.contact.photo, 'http://user2.photo');
			p.equals(response.contact.thumbnail, 'http://user2.thumbnail');
		})
		.and()
		.wait('user1', 'ContactChanged', (response) => {
			p.equals(response.contact.userId, userId2);
			p.equals(response.contact.status, map.ContactStatus.friend);
			p.equals(response.contact.name, 'User2');
			p.equals(response.contact.photo, 'http://user2.photo');
			p.equals(response.contact.thumbnail, 'http://user2.thumbnail');
		})
		.and()
		// user1 get Update because hi get system message that user2 was added to contact
		.wait('user1', 'Update', (response) => {
			p.equals(response.update.message.type, map.MessageType.systemNotification);
			p.equals(response.update.feedId, userId2);
		})
		// user2 did not receive ContactChanged because his contact not changed
		.and()
		// user1 get ChatUpdate because hi get system message that user2 was added to contact
		.wait('user1', 'ChatUpdate', (response) => {
			p.equals(response.update.top.type, map.MessageType.systemNotification);
			p.equals(response.update.feedId, userId2);
			p.equals(response.update.name, 'User2');
			p.equals(response.update.thumbnail, 'http://user2.thumbnail');
		})

		.then() // user1 add to contact user3

		.send('user1', 'AddContact', {contact: {userId: userId3}}, (response) => {
			p.equals(response.modelName, 'AddContactResp');
			p.equals(response.contact.userId, userId3);
			p.equals(response.contact.name, 'User3');
			p.equals(response.contact.photo, 'http://user3.photo');
			p.equals(response.contact.thumbnail, 'http://user3.thumbnail');
		})
		.wait('user1', 'ContactChanged', (response) => {
			p.equals(response.contact.userId, userId3);
			p.equals(response.contact.status, map.ContactStatus.friend);
			p.equals(response.contact.name, 'User3');
			p.equals(response.contact.photo, 'http://user3.photo');
			p.equals(response.contact.thumbnail, 'http://user3.thumbnail');
		})
		.and()
		// user1 get Update because hi get system message that user3 was added to contact
		.wait('user1', 'Update', (response) => {
			p.equals(response.update.message.type, map.MessageType.systemNotification);
			p.equals(response.update.feedId, userId3);
		})
		// user2 did not receive ContactChanged because his contact not changed
		.and()
		// user1 get ChatUpdate because hi get system message that user3 was added to contact
		.wait('user1', 'ChatUpdate', (response) => {
			p.equals(response.update.top.type, map.MessageType.systemNotification);
			p.equals(response.update.top.payload.indexOf('You added ') != -1, true);
		})
		.and()
		.wait('user3', 'ContactChanged', (response) => {
			p.equals(response.contact.userId, userId1);
			p.equals(response.contact.status, map.ContactStatus.pending);
		})

		.then() // user1 update profile

		.send('user1', 'UpdateUser', {user: {
				name: 'User1(updated)',
				photo: 'http://user1.photo.updated',
				thumbnail: 'http://user1.thumbnail.updated'
			}}, (response) => {
			p.equals(response.modelName, 'UserResp');
			p.equals(response.user.name, 'User1(updated)');
			p.equals(response.user.photo, 'http://user1.photo.updated');
			p.equals(response.user.thumbnail, 'http://user1.thumbnail.updated');
		})
		.and()
		.wait('user3', 'ContactChanged', (response) => {
			p.equals(response.contact.name, imDirectoryIsTurnedOn ? 'User1(updated)' : 'User1');
			p.equals(response.contact.photo, 'http://user1.photo.updated');
			p.equals(response.contact.thumbnail, 'http://user1.thumbnail.updated');
		})
		.and()
		.wait('user2', 'ContactChanged', (response) => {
			p.equals(response.contact.name, imDirectoryIsTurnedOn ? 'User1(updated)' : 'User1');
			p.equals(response.contact.photo, 'http://user1.photo.updated');
			p.equals(response.contact.thumbnail, 'http://user1.thumbnail.updated');
		})
 		.and()
		.wait('user2', 'ChatUpdate', (response) => {
			p.equals(response.update.feedId, userId1);
			p.equals(response.update.name, imDirectoryIsTurnedOn ? 'User1(updated)' : 'User1');
			p.equals(response.update.thumbnail, 'http://user1.thumbnail.updated');
		})

		.then() // user2 block user1

		.send('user2', 'BlockContact', {userId: userId1}, (response) => {
			p.equals(response.modelName, 'BlockContactResp');
			p.equals(response.contact.userId, userId1);
			p.equals(response.contact.name, imDirectoryIsTurnedOn ? 'User1(updated)' : 'User1');
			p.equals(response.contact.photo, 'http://user1.photo.updated');
			p.equals(response.contact.thumbnail, 'http://user1.thumbnail.updated');
		})
		.and()
		.wait('user2', 'ContactChanged', (response) => {
			p.equals(response.contact.userId, userId1);
			p.equals(response.contact.status, map.ContactStatus.blocked);
			p.equals(response.contact.name, imDirectoryIsTurnedOn ? 'User1(updated)' : 'User1');
			p.equals(response.contact.photo, 'http://user1.photo.updated');
			p.equals(response.contact.thumbnail, 'http://user1.thumbnail.updated');
		})
		.and()
		.wait('user2', 'ChatUpdate', (response) => {
			p.equals(response.update.feedId, userId1);
			p.equals(response.update.name, imDirectoryIsTurnedOn ? 'User1(updated)' : 'User1');
			p.equals(response.update.thumbnail, 'http://user1.thumbnail.updated');
			p.equals(response.update.top.systemMessageType, 'system.you.block.user');
		})
		.and()
		.wait('user1', 'ContactChanged', (response) => {
			p.equals(response.contact.userId, userId2);
			p.equals(response.contact.status, map.ContactStatus.iAmBlocked);
			p.equals(response.contact.name, 'User2');
			p.equals(response.contact.photo, 'http://user2.photo');
			p.equals(response.contact.thumbnail, 'http://user2.thumbnail');
		})
		.and()
		.wait('user1', 'ChatUpdate', (response) => {
			p.equals(response.update.feedId, userId2);
			p.equals(response.update.name, 'User2');
			p.equals(response.update.thumbnail, 'http://user2.thumbnail');
			p.equals(response.update.top.systemMessageType, 'system.you.blocked.by.user');
    })
    .then()
    .timeout(500)

		.then() // user2 unblock user1

		.send('user2', 'UnBlockContact', {userId: userId1}, (response) => {
			p.equals(response.modelName, 'UnBlockContactResp');
			p.equals(response.contact.userId, userId1);
			p.equals(response.contact.status, map.ContactStatus.friend);
			p.equals(response.contact.name, imDirectoryIsTurnedOn ? 'User1(updated)' : 'User1');
			p.equals(response.contact.photo, 'http://user1.photo.updated');
			p.equals(response.contact.thumbnail, 'http://user1.thumbnail.updated');
		})
		.and()
		.wait('user2', 'ContactChanged', (response) => {
			p.equals(response.contact.userId, userId1);
			p.equals(response.contact.status, map.ContactStatus.friend);
			p.equals(response.contact.name, imDirectoryIsTurnedOn ? 'User1(updated)' : 'User1');
			p.equals(response.contact.photo, 'http://user1.photo.updated');
			p.equals(response.contact.thumbnail, 'http://user1.thumbnail.updated');
		})
		.and()
		.wait('user2', 'ChatUpdate', (response) => {
			p.equals(response.update.feedId, userId1);
			p.equals(response.update.name, imDirectoryIsTurnedOn ? 'User1(updated)' : 'User1');
			p.equals(response.update.thumbnail, 'http://user1.thumbnail.updated');
			p.equals(response.update.top.systemMessageType, 'system.you.unblock.user');
		})
		.and()
		.wait('user1', 'ContactChanged', (response) => {
			p.equals(response.contact.userId, userId2);
			p.equals(response.contact.status, map.ContactStatus.friend);
			p.equals(response.contact.name, 'User2');
			p.equals(response.contact.photo, 'http://user2.photo');
			p.equals(response.contact.thumbnail, 'http://user2.thumbnail');
		})
		.and()
		.wait('user1', 'ChatUpdate', (response) => {
			p.equals(response.update.feedId, userId2);
	  	p.equals(response.update.name, 'User2');
	  	p.equals(response.update.thumbnail, 'http://user2.thumbnail');
	  	p.equals(response.update.top.systemMessageType, 'system.user.unblock.you');
		})
}

