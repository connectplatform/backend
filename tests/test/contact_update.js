const contactStatusHelper = require('./helpers/contact.helper');

module.exports = function (p) {
	const h = contactStatusHelper();
	const s = map.ContactStatus;

	h.execute(p, [s.unknown], h.handler(p, 'UpdateContact', 'ErrorResp', global.ErrorCode.notFound));
	h.execute(p, [s.pending], h.handler(p, 'UpdateContact', 'ErrorResp', global.ErrorCode.notFound));
	h.execute(p, [s.friend], h.handler(p, 'UpdateContact', 'UpdateContactResp', s.friend));
	h.execute(p, [s.blocked], h.handler(p, 'UpdateContact', 'ErrorResp', map.ErrorCode.notFound));
	h.execute(p, [s.friend, map.ContactStatus.blocked], h.handler(p, 'UpdateContact', 'UpdateContactResp', s.blocked));
	h.execute(p, [s.friend, map.ContactStatus.iAmBlocked], h.handler(p, 'UpdateContact', 'UpdateContactResp', s.iAmBlocked));
	h.execute(p, [s.friend, map.ContactStatus.spammer], h.handler(p, 'UpdateContact', 'UpdateContactResp', s.spammer));
	h.execute(p, [s.friend, map.ContactStatus.iAmSpammer], h.handler(p, 'UpdateContact', 'UpdateContactResp', s.iAmSpammer));
};
