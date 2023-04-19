const Transponder = require('@webcobra/transponder');
const rxjs = require('rxjs');
const operators = require('rxjs/operators');
const OverwriteEnvService = require('./env.js');

module.exports = function (config) {
	const self = {};
	const env = new OverwriteEnvService(config);
	
	console.log('api http: ', env.getHttpUrl());
	console.log('api socket: ', env.getSocketUrl('token'));
	
	const formatToken = (token) => {
		if (['user1', 'user2', 'user3', 'user4', 'user5', 'user6'].indexOf(token) !== -1) {
			token = parseInt(token[token.length - 1]);
		}
		return token;
	};
	
	// Hydrator
	self.hydrator = new Transponder.HydratorService();
	
	// Env
	self.env = env;
	
	// Http
	const HttpTransponderService = Transponder.HttpTransponderService;
	HttpTransponderService.prototype.loadProtocol = function () {
		this.hydrator.setMap(config.map);
		this.mapLoaded$.next(true);
		
		return rxjs.of(config.map);
	};
	const httpTransponderService = new HttpTransponderService(env);
	httpTransponderService.loadProtocol().subscribe();
	self.http = httpTransponderService;
	
	// WS
	self.socket = {};
	const connectionsMap = new Map();
	
	self.socket.connect = (token, callback) => {
		token = formatToken(token);
		const socket = new Transponder.SocketTransponderService(env);
		socket._connect(
			rxjs.of(token).pipe(
				operators.tap(() => {
					connectionsMap.set(token, socket);
					if (callback && typeof callback === 'function') {
						callback();
					}
				})
			)
		);
	};
	
	self.socket.send = (token, modelName, data, callback) => {
		token = formatToken(token);
		socket = connectionsMap.get(token);
		if (!socket) {
			throw new Error(`No WS connection for token ${token}`);
		}
		const ref = +new Date();
		socket.on(modelName, ref)
			.pipe(
				operators.catchError(error => rxjs.of(error)),
				operators.take(1)
			)
			.subscribe(response => {
				if (callback && typeof callback === 'function') {
					callback(response);
				}
			});
		
		socket.send(modelName, data, ref);
	};
	
	self.socket.wait = (token, modelName, callback) => {
		token = formatToken(token);
		socket = connectionsMap.get(token);
		if (!socket) {
			throw new Error(`No WS connection for token ${token}`);
		}
		socket.on(modelName)
			.pipe(
				operators.catchError(error => rxjs.of(error)),
				operators.take(1)
			)
			.subscribe(response => {
				if (callback && typeof callback === 'function') {
					callback(response);
				}
			});
	};
	
	self.socket.close = (token, callback) => {
		token = formatToken(token);
		socket = connectionsMap.get(token);
		if (socket) {
			socket.allowReconnection = false;
			socket.reconnection$ = true; // hack to disable reconnection
			socket.websocket$.complete();
			socket.websocket$.unsubscribe();
			connectionsMap.delete(token);
			if (callback && typeof callback === 'function') {
				callback();
			}
		}
	};
	
	return self;
};

