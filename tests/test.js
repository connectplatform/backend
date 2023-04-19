const fs = require('fs');
const util = require('util');
const rxjs = require('rxjs');
const operators = require('rxjs/operators');
const config = JSON.parse(fs.readFileSync(__dirname + '/config/config.json', 'utf8'));
const mapResult = getMap();
const transponderNodePolyfills = require('@webcobra/transponder-node-polyfill').transponderNodePolyfills;
transponderNodePolyfills();

// Globals
global.config = config;
global.frameworkDir = __dirname + '/framework';
global.entityHelper = require(global.frameworkDir + '/helpers/EntityHelper');
global.fixtureData = require(global.frameworkDir + '/data/FixtureData');
global.protocolVersion = mapResult.version === 'base' ? null : 'v' + mapResult.version;
global.map = mapResult.map;
global.ErrorCode = global.map.ErrorCode;
global.transponder = require(__dirname + '/framework/transponder/transponder.js')({
	id: null,
	ssl: false,
	production: true,
	debug: global.config.debug,
	host: config.host,
	map: mapResult.map,
	protocolVersion: global.protocolVersion
});

const framework = require(global.frameworkDir + '/App')({
	dbName: global.config.dbName,
	elastic_search: global.config.elastic_search,
	dumpsDir: __dirname + '/../test/db_dump'
});

/* Add Commands */
framework.addCommand('changeTime', function (timestamp, callback) {
	transponder.http.send('ChangeCronTime', {timestamp}).subscribe(response => callback(response));
});
framework.addCommand('sendHttp', function (identifier, messageName, messageData, callback) {
	const token = global.fixtureData.getToken(identifier);
	if (global.config.debug) {
		console.log('[HTTP] Sending message: ', messageName, util.inspect(messageData, false, 999));
	}
	transponder.http.send(messageName, messageData, token)
		.pipe(
			operators.catchError(error => rxjs.of(error)),
			operators.take(1)
		)
		.subscribe(response => callback(response));
});
framework.addCommand('sendGetUser', function (token, callback) {
	transponder.http.send('User', {}, token).subscribe(response => callback(response));
});
framework.addCommand('sendUpdateUser', function (token, user, callback) {
	transponder.http.send('User', {user}, token).subscribe(response => callback(response));
});
framework.addCommand('clearState', function (callback) {
	transponder.http.send('ClearState', {}).subscribe(response => {
		callback(response)
	});
});
framework.addCommand('connect', transponder.socket.connect);
framework.addCommand('send', transponder.socket.send);
framework.addCommand('wait', transponder.socket.wait);
framework.addCommand('close', transponder.socket.close);

setTimeout(function () {
	const test = require('tape');
	const timeout = 15000;
	const callback = function (path) {
		return function (t) {
			console.log("########################################################################");
			pipeline = framework.test(t);
			require('./test/' + path)(pipeline);
			pipeline.run()
		}
	};

	if (process.env.SKIP_MAIN != '1' || process.env.RUN_ALL == '1') {
		test('Login', {timeout: timeout}, callback('login'));
		test('LoginFacebook', {timeout: timeout}, callback('login_facebook'));
		test('Device', {timeout: timeout}, callback('device'));
		test('UpdateUser', {timeout: timeout}, callback('update_user'));
		test('WebSocket', {timeout: timeout}, callback('websocket'));
		test('ACL', {timeout: timeout}, callback('acl'));
		test('UpdateFeed', {timeout: timeout}, callback('update_feed'));
		test('UpdateFeedRoom', {timeout: timeout}, callback('update_feed_room'));
		test('PointerRoom', {timeout: timeout}, callback('pointer_room'));
		test('Retrieve', {timeout: timeout}, callback('retrieve'));
		test('ChatUpdate', {timeout: timeout}, callback('chat_update'));
		test('ChatUpdateRoom', {timeout: timeout}, callback('chat_update_room'));
		test('Room', {timeout: timeout}, callback('room'));
		test('MessageReply', {timeout: timeout}, callback('message_reply'));
		test('Circle', {timeout: timeout}, callback('circle'));
		test('Task', {timeout: timeout}, callback('task'));
		test('Workflow', {timeout: timeout}, callback('workflow'));
		test('AppConfBots', {timeout: timeout}, callback('app_conf_bots'));
		test('AppConfContacts', {timeout: timeout}, callback('app_conf_contacts'));
		test('AppConfRolesAndPermissions', {timeout: timeout}, callback('app_conf_roles_and_permissions'));
		test('OpenGraph', {timeout: timeout}, callback('open_graph'));
    test('Synapse', {timeout: timeout}, callback('synapse'));
		test('Bot', {timeout: timeout}, callback('bot'));
		test('BotSend', {timeout: timeout}, callback('bot_send'));

    // temporarily skipped:
		// test('Star', {timeout: timeout}, callback('star'))
    // test('UserStatus', {timeout: timeout}, callback('user_status'))
    // test('PointerChat', {timeout: timeout}, callback('pointer_chat'))
    // test('Call', {timeout: timeout}, callback('call'));
		// test('CallRoom', {timeout: timeout}, callback('call_room'));
    // test('Media', {timeout: timeout}, callback('media'));
    // test('CSR', {timeout: timeout}, callback('csr'));
		// test('CSR ACL', {timeout: timeout}, callback('csr_acl'));
	}

	if (process.env.RUN_ELASTICA == '1' || process.env.RUN_ALL == '1') {
		test('FindUser', {timeout: timeout}, callback('find_user'));
	}

	if (process.env.RUN_CMS == '1' || process.env.RUN_ALL == '1') {
		test('FeedPost', {timeout: timeout}, callback('feed_post'));
		test('FeedPostACL', {timeout: timeout}, callback('feed_post_acl'));
		test('LocalizedStore', {timeout: timeout}, callback('localized_store'));
		test('Like', {timeout: timeout}, callback('like'));
		test('Order', {timeout: timeout}, callback('order'));
		test('OrderTicket', {timeout: timeout}, callback('order_ticket'));
		test('OrderTicketWithTime', {timeout: timeout}, callback('order_ticket_with_time'));
		test('ArchiveExpiredTickets', {timeout: timeout}, callback('order_archive_expired_tickets'));
		test('OrderBuyNow', {timeout: timeout}, callback('order_buy_now'));
		test('VendorStatistics', {timeout: timeout}, callback('vendor_statistics'));
		test('OrderQuantityLimit', {timeout: timeout}, callback('order_quantity_limit'));
		test('OrderAvailableQuantity', {timeout: timeout}, callback('order_available_quantity'));
		test('OrdersMerge', {timeout: timeout}, callback('order_merge'));
	}

	if (process.env.RUN_CONTACTS == '1' || process.env.RUN_ALL == '1') {
		// test('ContactBio', {timeout: timeout}, callback('contact_bio'));
		// test('ContactUpdate', {timeout: timeout}, callback('contact_update'));
		// test('ContactAdd', {timeout: timeout}, callback('contact_add'));
		// test('ContactDelete', {timeout: timeout}, callback('contact_delete'));
		// test('ContactBlock', {timeout: timeout}, callback('contact_block'));
		// test('ContactUnblock', {timeout: timeout}, callback('contact_unblock'));
		test('ContactBlockUnblock', {timeout: timeout}, callback('contact_block_unblock'));
		// test('ContactToSpam', {timeout: timeout}, callback('contact_to_spam'));
		// test('ContactSync', {timeout: timeout}, callback('contact_sync'));
		// test('ContactChanged', {timeout: timeout}, callback('contact_changed'));
	}
}, 5000);

function getMap() {
	let version = parseVersionParam();
	const protocols = readProtocols();
	// console.log('protocols', protocols);
	if (!protocols.length) {
		throw new Error('No protocols in protocols dir');
	}

	let protocol = null;
	if (version) {
		protocol = protocols.find(p => p.version.toString() === version.toString());
		if (!protocol) {
			throw new Error(`Protocol of "${version}" not found`);
		}
	} else {
		// version = Math.max(...versions);
		version = 'base';
		protocol = protocols.find(p => p.version.toString() === version.toString());
	}

	if (!protocol) {
		throw new Error(`Protocol of version "${version}" not found in protocols dir`);
	}

	console.log('Loaded protocol for version: ' + version);

	return {
		version,
		path: __dirname + '/protocols/' + protocol.fileName,
		map: JSON.parse(fs.readFileSync(__dirname + '/protocols/' + protocol.fileName, 'utf8'))
	};
}

function parseVersionParam() {
	const params = JSON.parse(process.env.npm_config_argv);
	let versionParam = null;
	if (params.hasOwnProperty('remain') && Array.isArray(params.remain) && params.remain.length) {
		params.remain.forEach(param => {
			const regex = /v(\d)/gm;

			if (regex.test(param)) {
				versionParam = parseInt(param.replace(regex, `$1`));
			}
		})
	}

	return versionParam ? versionParam : null;
}

function readProtocols() {
	const protocols = [];
	const files = fs.readdirSync('./protocols');
	files.forEach(fileName => {
		fileName = fileName.trim();
		if (fileName === 'map_base.json') {
			protocols.push({version: 'base', fileName})
		} else if (/map_v(.*).json/gm.test(fileName)) {
			protocols.push({version: fileName.replace(/map_v(.*).json/gm, `$1`), fileName});
		}
	});

	return protocols;
}
