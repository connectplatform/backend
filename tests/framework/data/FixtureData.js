module.exports = (function () {
	
	const userData = {
		'user1': {
			'id': '000000000000000000000001',
			'token': '1'
		},
		'user2': {
			'id': '000000000000000000000002',
			'token': '2'
		},
		'user3': {
			'id': '000000000000000000000003',
			'token': '3'
		},
		'user4': {
			'id': '000000000000000000000004',
			'token': '4'
		},
		'user5': {
			'id': '000000000000000000000005',
			'token': '5'
		},
		'user6': {
			'id': '000000000000000000000006',
			'token': '6'
		}
	};
	
	this.getToken = function (v) {
		if (userData[v] && userData[v].token) {
			return userData[v].token
		} else {
			return v
		}
	};
	
	this.get = function (userAlias) {
		if (userData[userAlias]) {
			return userData[userAlias]
		} else {
			return false
		}
	};
	
	this.getUserId = function (userAlias) {
		if (userData[userAlias]) {
			return userData[userAlias].id
		} else {
			return false
		}
	};
	
	return {
		get: this.get,
		getToken: this.getToken,
		getUserId: this.getUserId,
	}
})();
