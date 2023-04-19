const contactStatusHelper = require('./helpers/contact.helper');

module.exports = function (p) {
	const h = contactStatusHelper();
	const s = map.ContactStatus;
	const permissionDenied = map.ErrorCode.permissionDenied;

	h.execute(p, [s.unknown], h.handler(p, 'DeleteContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.pending], h.handler(p, 'DeleteContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.blocked], h.handler(p, 'DeleteContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.iAmBlocked], h.handler(p, 'DeleteContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.spammer], h.handler(p, 'DeleteContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.iAmSpammer], h.handler(p, 'DeleteContact', 'ErrorResp', permissionDenied));

	h.execute(p, [s.friend], h.handler(p, 'DeleteContact', 'DeleteContactResp', s.unknown));
	h.execute(p, [s.pending, s.friend], h.handler(p, 'DeleteContact', 'DeleteContactResp', s.pending));
	h.execute(p, [s.friend, s.blocked], h.handler(p, 'DeleteContact', 'DeleteContactResp', s.blocked));
	h.execute(p, [s.friend, s.iAmBlocked], h.handler(p, 'DeleteContact', 'DeleteContactResp', s.iAmBlocked));
	h.execute(p, [s.friend, s.spammer], h.handler(p, 'DeleteContact', 'DeleteContactResp', s.spammer));
	h.execute(p, [s.friend, s.iAmSpammer], h.handler(p, 'DeleteContact', 'DeleteContactResp', s.iAmSpammer));
};
