const contactStatusHelper = require('./helpers/contact.helper');

module.exports = function (p) {
	const helper = contactStatusHelper();
	const s = map.ContactStatus;
	const permissionDenied = map.ErrorCode.permissionDenied;
	
	helper.execute(p, [s.unknown], helper.handler(p, 'AddContact', 'AddContactResp', s.friend));
	helper.execute(p, [s.pending], helper.handler(p, 'AddContact', 'AddContactResp', s.friend));
	helper.execute(p, [s.friend], helper.handler(p, 'AddContact', 'ErrorResp', permissionDenied));
	helper.execute(p, [s.blocked], helper.handler(p, 'AddContact', 'ErrorResp', permissionDenied));
	helper.execute(p, [s.iAmBlocked], helper.handler(p, 'AddContact', 'ErrorResp', permissionDenied));
	helper.execute(p, [s.spammer], helper.handler(p, 'AddContact', 'ErrorResp', permissionDenied));
	helper.execute(p, [s.iAmSpammer], helper.handler(p, 'AddContact', 'ErrorResp', permissionDenied));
}
