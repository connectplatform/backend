const contactStatusHelper = require('./helpers/contact.helper');

module.exports = function (p) {
	const h = contactStatusHelper();
	const s = map.ContactStatus;
	const permissionDenied = map.ErrorCode.permissionDenied;

	h.execute(p, [s.unknown], 		h.handler(p, 'UnBlockContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.pending], 		h.handler(p, 'UnBlockContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.friend], 	 		h.handler(p, 'UnBlockContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.blocked], 		h.handler(p, 'UnBlockContact', 'UnBlockContactResp', s.unknown));
	h.execute(p, [s.iAmBlocked], 	h.handler(p, 'UnBlockContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.spammer], 		h.handler(p, 'UnBlockContact', 'ErrorResp', permissionDenied));
	h.execute(p, [s.iAmSpammer], 	h.handler(p, 'UnBlockContact', 'ErrorResp', permissionDenied));
};
