const UserHelper = require('./helpers/user.helper');

module.exports = function (p) {
	
	const userHelper = new UserHelper(p);
	
	p.connect('user1').then();
	
	const users = [
		{name: 'vendor_1_vendor', phone: '+380930001001', isVendor: true, active: true, roles: ['vendor']},
		{name: 'vendor_2_vendor', phone: '+380930002001', isVendor: true, active: true, roles: ['vendor']},

		// {name: 'vendor_1_vendor_staff', phone: '+380930001002', isVendor: false, active: true, roles: ['vendor_staff']},
		// {name: 'vendor_2_vendor_staff', phone: '+380930002002', isVendor: false, active: true, roles: ['vendor_staff']},
		// {name: 'vendor_1_vendor_rep', phone: '+380930001003', isVendor: false, active: true, roles: ['vendor_rep']},
		// {name: 'vendor_2_vendor_rep', phone: '+380930002003', isVendor: false, active: true, roles: ['vendor_rep']},

		{name: 'customer', phone: '+380930003001', isVendor: false, active: true, roles: ['customer']},
	];
	users.forEach(u => userHelper.createUser(u));
	users.forEach(user => {
			p.then();
			user.id = get(userHelper.getIdKey(user));
			if (user.isVendor) {
				user.vendorId = get(userHelper.getIdKey(user));
			} else if (user.roles.filter(role => ['vendor_staff', 'vendor_rep'].includes(role)).length) {
				const vendorUser = users.find(u => u.name === user.name.replace(user.roles[0], 'vendor'));
				user.vendorId = get(userHelper.getIdKey(vendorUser));
			}

			p.send('user1', 'UpdateUser', {user}, function (response) {
				p.equals(response.modelName, 'UserResp');
			});
		});
	
	vendor1IdKey = () => userHelper.getIdKey(users.find(user => user.name === 'vendor_1_vendor'));
	vendor1TokenKey = () => userHelper.getTokenKey(users.find(user => user.name === 'vendor_1_vendor'));
	vendor2IdKey = () => userHelper.getIdKey(users.find(user => user.name === 'vendor_2_vendor'));
	vendor2TokenKey = () => userHelper.getTokenKey(users.find(user => user.name === 'vendor_2_vendor'));
	customerTokenKey = () => userHelper.getTokenKey(users.find(user => user.name === 'customer'));
	
	p
	.then()
	// create Request in vendor 1 scope
	.send('user1', 'GetCircle', {supervisor: 'testbot', params: 'action=buyCar&*carId=22', members: ['000000000000000000000003']}, response => {
	  p.equals(response.modelName, 'GetCircleResp');
	  p.set('vendor1_request_1.roomId', response.roomId)
	})
	.then()
	.send('user1', 'CreateCsr', {request: {
	  roomId: get('vendor1_request_1.roomId'),
	  userId: '000000000000000000000003', // Create request from user3
		vendorId: get(vendor1IdKey()),
	  descr: 'Request in vendor 1 scope'
	}}, response => {
	  p.set('vendor1_request_1.id', response.request.id);
	  p.set('vendor1_request_1.roomId', response.request.roomId);

	  p.equals(response.modelName, 'CreateCsrResp');
	  p.equals(response.request.userId, '000000000000000000000003');
	  p.equals(response.request.status, map.CsrStatus.open);
	  p.equals(response.request.descr, 'Request in vendor 1 scope');
	  p.deepEquals(response.request.tags, [])
	})

	.then()
	// create Request in vendor 2 scope
	.send('user1', 'GetCircle', {supervisor: 'testbot', params: 'action=buyCar&*carId=23', members: ['000000000000000000000003']}, response => {
		p.equals(response.modelName, 'GetCircleResp');
		p.set('vendor2_request_1.roomId', response.roomId)
	})
	.then()
	.send('user1', 'CreateCsr', {request: {
			roomId: get('vendor2_request_1.roomId'),
			userId: '000000000000000000000003', // Create request from user3
			vendorId: get(vendor2IdKey()),
			descr: 'Request in vendor 2 scope'
		}}, response => {
		p.set('vendor2_request_1.id', response.request.id);
		p.set('vendor2_request_1.roomId', response.request.roomId);

		p.equals(response.modelName, 'CreateCsrResp');
		p.equals(response.request.userId, '000000000000000000000003');
		p.equals(response.request.status, map.CsrStatus.open);
		p.equals(response.request.descr, 'Request in vendor 2 scope');
		p.deepEquals(response.request.tags, [])
	})

	// check that user with role "vendor" can only get his requests
	.then()
	.send(get(vendor1TokenKey()), 'OpenCsr', {count: -1}, response => {
		p.equals(response.modelName, 'OpenCsrResp');
		p.equals(response.requests.length, 1);
		p.equals(response.requests[0].vendorId, p.get(vendor1IdKey()));
		p.equals(response.requests[0].descr, 'Request in vendor 1 scope');
	})
	.then()
	.send(get(vendor2TokenKey()), 'OpenCsr', {count: -1}, response => {
		p.equals(response.modelName, 'OpenCsrResp');
		p.equals(response.requests.length, 1);
		p.equals(response.requests[0].vendorId, p.get(vendor2IdKey()));
		p.equals(response.requests[0].descr, 'Request in vendor 2 scope');
	})
	// check that user with role "super_admin" can get all requests
	.then()
	.send('user1', 'OpenCsr', {count: -1}, response => {
		p.equals(response.modelName, 'OpenCsrResp');
		p.equals(response.requests.length, 2);
	})
	// check that user with role "customer" can't get any requests
	.then()
	.send(get(customerTokenKey()), 'OpenCsr', {count: -1}, response => {
		p.equals(response.modelName, 'ErrorResp');
	})
};
