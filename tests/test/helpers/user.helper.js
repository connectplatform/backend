module.exports = function(p) {
	
	const getIdKey = function(user) {
		return 'user.' + user.name + '.id';
	};
	const getTokenKey = function(user) {
		return 'user.' + user.name + '.token';
	};
	
	return {
		getIdKey,
		getTokenKey,
		createUser: (user) => {
			const tokenKey = getTokenKey(user);
			const idKey = getIdKey(user);
			return p
				.sendHttp(null, 'RequestVerification', {phone: user.phone}, response => {
					p.equals(response.modelName, 'RequestVerificationResp')
				})
				.then()
				.sendHttp(null, 'ConfirmVerification', {
					phone: user.phone,
					smsCode: '111',
					deviceId: user.deviceId || '3310',
					deviceName: user.deviceName || 'nokia',
					os: user.os || map.Platform.ios
				}, function (response) {
					p.equals(response.modelName, 'ConfirmVerificationResp');
					p.set(tokenKey, response.token);
				})
				.then()
				.connect(get(tokenKey))
				.send(get('user.' + user.name + '.token'), 'User', {}, function (response) {
					p.equals(response.modelName, 'UserResp');
					p.set(idKey, response.user.id);
				})
				.then()
				.send(get(tokenKey), 'UpdateUser', {user}, function (response) {
					p.equals(response.modelName, 'UserResp');
				})
		},
		updateUser: (user) => {
			return p.send(get('user.' + user.name + '.token'), 'UpdateUser', {user}, function (response) {
				p.equals(response.modelName, 'UserResp');
			})
		}
	}
}

