const contactStatusHelper = require('./helpers/contact.helper');

module.exports = function (p) {
	const h = contactStatusHelper();
	const s = map.ContactStatus;
	const permissionDenied = map.ErrorCode.permissionDenied;

	h.execute(p, [s.unknown], h.handler(p, 'AddToSpam', 'AddToSpamResp', s.spammer));
	h.execute(p, [s.pending], h.handler(p, 'AddToSpam', 'AddToSpamResp', s.spammer));
	h.execute(p, [s.blocked], h.handler(p, 'AddToSpam', 'AddToSpamResp', s.spammer));
	h.execute(p, [s.iAmBlocked], h.handler(p, 'AddToSpam', 'AddToSpamResp', s.spammer));
	h.execute(p, [s.spammer], h.handler(p, 'AddToSpam', 'ErrorResp', permissionDenied));
	h.execute(p, [s.iAmSpammer], h.handler(p, 'AddToSpam', 'AddToSpamResp', s.spammer));
}
