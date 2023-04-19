module.exports = () => {
	this.user1 = {
		key: 'user1',
		phone: 380931000011
	};
	this.user2 = {
		key: 'user2',
		phone: 380932000011
	};

	this.createUser = (p, user) => {
		const phone = '+' + user.phone++;
		return p.sendHttp(null, 'RequestVerification', {phone: phone}, (response) => {
			p.equals(response.modelName, 'RequestVerificationResp')
		})
			.then()
			.sendHttp(null, 'ConfirmVerification', {
				phone: phone,
				smsCode: '111',
				deviceId: '3310',
				deviceName: 'nokia',
				os: map.Platform.ios
			}, (response) => {
				p.equals(response.modelName, 'ConfirmVerificationResp');
				p.equals(!!response.token, true);
				p.set(user.key + '.token', response.token);
			})
			.then()
			.sendHttp(get(user.key + '.token'), 'User', {}, (response) => {
				p.equals(response.modelName, 'UserResp');
				p.set(user.key + '.id', response.user.id);
			})
			.then();
	};

	this.initialize = (p) => {
		this.createUser(p, this.user1);
		this.createUser(p, this.user2);

		return p;
	};

	const unknown = (p) => {
		return p;
	};

	const pending = (p) => {
		return p.sendHttp(get('user2.token'), 'AddContact', {contact: {userId: get('user1.id')}}, (response) => {
			p.equals(response.modelName, 'AddContactResp');
			p.equals(response.contact.userId, p.get('user1.id'));
			p.equals(response.contact.status, map.ContactStatus.friend)
		});
	};

	const friend = (p) => {
		return p.sendHttp(get('user1.token'), 'AddContact', {contact: {userId: get('user2.id')}}, (response) => {
			p.equals(response.modelName, 'AddContactResp');
			p.equals(response.contact.userId, p.get('user2.id'));
			p.equals(response.contact.status, map.ContactStatus.friend)
		});
	};

	const blocked = (p) => {
		return p.sendHttp(get('user1.token'), 'BlockContact', {userId: get('user2.id')}, (response) => {
			p.equals(response.modelName, 'BlockContactResp');
			p.equals(response.contact.userId, p.get('user2.id'));
			p.equals(response.contact.status, map.ContactStatus.blocked)
		});
	};

	const iAmBlocked = (p) => {
		return p.sendHttp(get('user2.token'), 'BlockContact', {userId: get('user1.id')}, (response) => {
			p.equals(response.modelName, 'BlockContactResp');
			p.equals(response.contact.userId, p.get('user1.id'));
			p.equals(response.contact.status, map.ContactStatus.blocked)
		});
	};

	const spammer = (p) => {
		return p.sendHttp(get('user1.token'), 'AddToSpam', {userId: get('user2.id')}, (response) => {
			p.equals(response.modelName, 'AddToSpamResp');
			p.equals(response.contact.userId, p.get('user2.id'));
			p.equals(response.contact.status, map.ContactStatus.spammer)
		});
	};

	const iAmSpammer = (p) => {
		return p.sendHttp(get('user2.token'), 'AddToSpam', {userId: get('user1.id')}, (response) => {
			p.equals(response.modelName, 'AddToSpamResp');
			p.equals(response.contact.userId, p.get('user1.id'));
			p.equals(response.contact.status, map.ContactStatus.spammer)
		});
	};

	const contactRespHandler = (p, message, resultMessage, resultStatus) => {
		p.sendHttp(get('user1.token'), message, {userId: get('user2.id')}, (response) => {
			p.equals(response.modelName, resultMessage);
			p.equals(response.contact.userId, p.get('user2.id'));
			p.equals(response.contact.status, resultStatus);
		});
	};
	
	const updateContactHandler = (p, message, resultMessage, resultStatus) => {
		const name = 'status_' + resultStatus;
		p.sendHttp(get('user1.token'), message, {contact: {userId: get('user2.id'), name: name}}, (response) => {
			p.equals(response.modelName, resultMessage);
			p.equals(response.contact.userId, p.get('user2.id'));
			p.equals(response.contact.name, name);
			p.equals(response.contact.status, resultStatus);
		});
	};
	
	const addContactHandler = (p, message, resultMessage, resultStatus) => {
		p.sendHttp(get('user1.token'), message, {contact: {userId: get('user2.id')}}, (response) => {
			p.equals(response.modelName, resultMessage);
			p.equals(response.contact.userId, p.get('user2.id'));
			p.equals(response.contact.status, resultStatus);
		});
	};
	
	const errorRespHandler = (p, message, data, resultCode) => {
		p.sendHttp(get('user1.token'), message, data, (response) => {
			p.equals(response.modelName, 'ErrorResp');
			p.equals(response.code, resultCode);
		});
	};

	return {
		execute: (p, statuses, action) => {
			this.initialize(p);
			statuses.forEach(status => {
				switch (status) {
					case map.ContactStatus.unknown:
						unknown(p);
						break;
					case map.ContactStatus.pending:
						pending(p);
						break;
					case map.ContactStatus.friend:
						friend(p);
						break;
					case map.ContactStatus.blocked:
						blocked(p);
						break;
					case map.ContactStatus.iAmBlocked:
						iAmBlocked(p);
						break;
					case map.ContactStatus.spammer:
						spammer(p);
						break;
					case map.ContactStatus.iAmSpammer:
						iAmSpammer(p);
						break;
				}
				p.then();
			});

			return action.apply(null, [p]);
		},
		handler: (p, message, resultMessage, resultValue) => {
			if (message === 'AddContact' && resultMessage === 'AddContactResp') {
				return () => {
					addContactHandler(p, message, resultMessage, resultValue);
				}
			} else if (message === 'AddContact' && resultMessage === 'ErrorResp') {
				return () => {
					errorRespHandler(p, message, {contact: {userId: get('user2.id')}}, resultValue);
				}
			} else if (message === 'UpdateContact' && resultMessage === 'UpdateContactResp') {
				return () => {
					updateContactHandler(p, message, resultMessage, resultValue);
				}
			} else if(resultMessage === 'ErrorResp') {
				return () => {
					errorRespHandler(p, message, {userId: get('user2.id')}, resultValue);
				}
			} else {
				return () => {
					contactRespHandler(p, message, resultMessage, resultValue);
				}
			}
			p.then();
		}
	}
};
