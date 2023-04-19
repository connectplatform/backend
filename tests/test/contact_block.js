const contactStatusHelper = require('./helpers/contact.helper');

module.exports = function (p) {
	const h = contactStatusHelper();
	const s = map.ContactStatus;
	const permissionDenied = map.ErrorCode.permissionDenied;

	h.execute(p, [map.ContactStatus.unknown], 		h.handler(p, 'BlockContact', 'BlockContactResp', s.blocked));
	h.execute(p, [map.ContactStatus.pending], 		h.handler(p, 'BlockContact', 'BlockContactResp', s.blocked));
	h.execute(p, [map.ContactStatus.friend], 			h.handler(p, 'BlockContact', 'BlockContactResp', s.blocked));
	h.execute(p, [map.ContactStatus.blocked], 		h.handler(p, 'BlockContact', 'ErrorResp', permissionDenied));
	h.execute(p, [map.ContactStatus.iAmBlocked], 	h.handler(p, 'BlockContact', 'BlockContactResp', s.blocked));
	h.execute(p, [map.ContactStatus.spammer], 		h.handler(p, 'BlockContact', 'ErrorResp', permissionDenied));
	h.execute(p, [map.ContactStatus.iAmSpammer], h.handler(p, 'BlockContact', 'ErrorResp', permissionDenied));
};
