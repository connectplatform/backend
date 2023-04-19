const AppConfHelper = require('./helpers/app_conf.helper');

module.exports = function (p) {
	const helper = AppConfHelper();
	const testTimeout = 2000;
	
	const json1 = {
		"roles": [
			{
				"id": "custom_role_1",
				"name": "Custom Role 1",
				"permissions": [
					"perm1"
				]
			},
			{
				"id": "custom_role_2",
				"name": "Custom Role 2",
				"permissions": [
					"perm2"
				]
			}
		],
		"permissions": [
			{
				"id": "perm1",
				"name": "Perm 1"
			},
			{
				"id": "perm2",
				"name": "Perm 2"
			}
		],
	};
	
	const json2 = {
		"roles": [
			{
				"id": "custom_role_1",
				"name": "Custom Role 1",
				"permissions": ["perm1", "perm2", "perm3"]
			},
		],
		"permissions": [
			{
				"id": "perm3",
				"name": "Perm 3"
			}
		],
	};
	
	p
		.then(() => helper.updateFile(json1))
		.timeout(testTimeout)
		.then()
		.sendHttp('user1', 'UserRoles', {}, response => {
			p.equals(response.modelName, 'UserRolesResp');
			p.equals(!!response.roles.length, true);
			const role1 = response.roles && response.roles.filter(role => role.id === 'custom_role_1').length ? response.roles.filter(role => role.id === 'custom_role_1')[0] : null;
			p.equals(role1.id, 'custom_role_1');
			p.equals(role1.name, 'Custom Role 1');
			p.equals(role1.name, 'Custom Role 1');
			p.equals(role1.perms.length, 1);
			p.equals(!!role1.perms.find(r => r.id === 'perm1' && r.name === 'Perm 1'), true);
			
			const role2 = response.roles && response.roles.filter(role => role.id === 'custom_role_2').length ? response.roles.filter(role => role.id === 'custom_role_2')[0] : null;
			p.equals(role2.id, 'custom_role_2');
			p.equals(role2.name, 'Custom Role 2');
			p.equals(role2.name, 'Custom Role 2');
			p.equals(role2.perms.length, 1);
			p.equals(!!role2.perms.find(r => r.id === 'perm2' && r.name === 'Perm 2'), true);
		})
		.then(() => helper.updateFile(json2))
		.timeout(testTimeout)
		.then()
		.sendHttp('user1', 'UserRoles', {}, response => {
			p.equals(response.modelName, 'UserRolesResp');
			p.equals(!!response.roles.length, true);
			const role1 = response.roles && response.roles.filter(role => role.id === 'custom_role_1').length ? response.roles.filter(role => role.id === 'custom_role_1')[0] : null;
			p.equals(role1.id, 'custom_role_1');
			p.equals(role1.name, 'Custom Role 1');
			p.equals(role1.name, 'Custom Role 1');
			p.equals(role1.perms.length, 3);
			p.equals(!!role1.perms.find(r => r.id === 'perm1' && r.name === 'Perm 1'), true);
			p.equals(!!role1.perms.find(r => r.id === 'perm2' && r.name === 'Perm 2'), true);
			p.equals(!!role1.perms.find(r => r.id === 'perm3' && r.name === 'Perm 3'), true);
			
			const role2 = response.roles && response.roles.filter(role => role.id === 'custom_role_2').length ? response.roles.filter(role => role.id === 'custom_role_2')[0] : null;
			p.equals(role2.id, 'custom_role_2');
			p.equals(role2.name, 'Custom Role 2');
			p.equals(role2.name, 'Custom Role 2');
			p.equals(role2.perms.length, 1);
			p.equals(!!role2.perms.find(r => r.id === 'perm2' && r.name === 'Perm 2'), true);
		})
};
