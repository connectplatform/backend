var FrameworkPipelineStateFuture = require('./FrameworkPipelineStateFuture');
var fixture = require('./Fixture');
var _ = require('underscore');

module.exports = function (f, t) {
	var running = true;
	var parameters = {};
	
	var commands = {
		'then': null,
		'and': null
	};
	
	var variables = {};
	
	var chain = [];
	var scope = [];
	var plan = 0;
	
	var passes = [];
	
	this.connectedTokens = [];
	
	var init = function () {
		this.addCommand('then',);
		this.addCommand('set', function () {
			console.log('set')
		});
		this.addCommand('and', function () {
			console.log('and')
		});
		this.addCommand('wait', function () {
			console.log('Wait empty callback')
		});
		this.addCommand('timeout', function () {
			console.log('timeout empty callback')
		});
		this.addCommand('connect', function () {
		});
		this.addCommand('close', function () {
		});
		
		return this
	};
	
	const replaceArguments = function (args) {
		if (!args || typeof args !== 'object') {
			return args;
		}
		
		for (x in args) {
			const arg = args[x];
			if (!arg || typeof arg !== 'object') {
				continue;
			}
			if (arg.type == 'FrameworkPipelineStateFuture') {
				args[x] = arg.fun ? (this[arg.fun] ? this[arg.fun](arg.name) : arg.fun(arg.name)) : get(arg.name);
			} else {
				args[x] = replaceArguments(arg);
			}
		}
		
		return args;
	};
	
	this.parameters = function (_parameters) {
		parameters = _parameters;
		
		return this
	};
	
	this.addCommand = function (name, handler) {
		if (typeof handler == 'function') {
			commands[name] = handler
		}
		
		this[name] = function () {
			// console.log('ADD_COMMAND: ', util.inspect(arguments, false, 99))
			
			if (name === 'set') {
				if (!running) {
					variables[arguments[0]] = arguments[1];
				}
			}
			if (typeof arguments[arguments.length - 1] == 'function') {
				var callbackFunction = arguments[arguments.length - 1];
				var count = (callbackFunction.toString().split("p.equals(").length - 1)
					+ (callbackFunction.toString().split("p.deepEquals(").length - 1)
					+ (callbackFunction.toString().split("p.pass(").length - 1);
				var passesLength = (parameters.passes && parameters.passes.length) ? parameters.passes.length : 1;
				plan += count * passesLength;
				t.plan(plan)
			}
			
			chain.push({command: name, arguments: Array.prototype.slice.call(arguments)});
			
			return this
		}
	};
	
	this.run = function (callback) {
		// console.log('CHAIN: ', util.inspect(chain, false, 99));
		
		chain.push({command: 'then', arguments: []});
		chain.push({command: 'end', arguments: []});
		
		passes = parameters.passes;
		runPass()
	};
	
	var clearPrevPass = function (callback) {
		scope = [];
		variables = {};
		running = true;
		fixture.clearDB(f.params.dbName);
		if (this.clearElasticSearch === true) {
			this.clearElasticSearch = false;
			fixture.clearElasticDB(
				f.params.elastic_search.host,
				f.params.elastic_search.port,
				f.params.elastic_search.index,
				f.params.elastic_search.doc
			)
		}
		if (this.clearState && typeof this.clearState == 'function') {
			commands['clearState'](function () {
				callback()
			})
		} else {
			callback()
		}
	};
	
	var runPass = function () {
		clearPrevPass(function () {
			// if (parameters.fixtures) {
			fixture.apply(f.params.dumpsDir);
			// }
			
			if (passes && passes.length && passes[0]) {
				for (argumentName in passes[0]) {
					this.set(argumentName, passes[0][argumentName])
				}
				passes.splice(0, 1)
			}
			
			
			process(0)
		})
	};
	
	var process = function (x) {
		if (x >= chain.length) {
			callback()
		} else if (chain[x].command === 'timeout') {
			setTimeout(function () {
				process(x + 1)
			}, chain[x].arguments[0])
		} else if (chain[x].command === 'set') {
			if (chain[x].arguments[1] && {}.toString.call(chain[x].arguments[1]) === '[object Function]') {
				variables[chain[x].arguments[0]] = chain[x].arguments[1]()
			} else {
				variables[chain[x].arguments[0]] = chain[x].arguments[1]
			}
			process(x + 1)
		} else if (chain[x].command === 'then') {
			var promises = [];
			var scopeWaits = [];
			var scopeOthers = [];
			var handler = chain[x].arguments[0] && typeof chain[x].arguments[0] === 'function' ? chain[x].arguments[0] : null;
			
			running = false;
			
			for (y = 0; y < scope.length; y++) {
				if (scope[y].command === 'wait') {
					scopeWaits.push(scope[y])
				} else {
					scopeOthers.push(scope[y])
				}
			}
			
			scope = scopeWaits.concat(scopeOthers);
			
			// console.log('SCOPE: ', scope);
			
			for (y = 0; y < scope.length; y++) {
				let args = scope[y].arguments;
				const userCallback = args[args.length - 1];
				const promise = new Promise((resolve, reject) => {
					// replace parameters
					args = replaceArguments(args);
					if (typeof (userCallback) == 'function') {
						args[args.length - 1] = (response, error) => {
							if (error) {
								return reject(error);
							}
							userCallback(response);
							resolve(response)
						}
					}
					commands[scope[y].command].apply(null, args)
				});
				promises.push(promise);
			}
			
			Promise.all(promises).then((response) => {
				if (handler) {
					handler(response);
				}
				scope = [];
				running = true;
				process(x + 1)
			})
		} else if (chain[x].command === 'close') {
			running = false;
			var token = replaceArguments(chain[x].arguments)[0];
			running = true;
			
			if (token && this.connectedTokens.indexOf(token) !== -1) {
				transponder.socket.close(token, () => {
					this.connectedTokens = this.connectedTokens.filter(t => t !== token);
					process(x + 1);
				})
			} else {
				process(x + 1)
			}
		} else if (chain[x].command === 'connect') {
			running = false;
			var args = replaceArguments(chain[x].arguments);
			funning = true;
			var token = args[0];
			var cb = args[1];
			if (token && this.connectedTokens.indexOf(token) === -1) {
				transponder.socket.connect(token, () => {
					this.connectedTokens.push(token);
					if (typeof cb === 'function') {
						cb();
					}
					process(x + 1)
				})
			} else {
				process(x + 1)
			}
		} else if (chain[x].command === 'and') {
			running = true;
			process(x + 1)
		} else if (chain[x].command === 'end') {
			if (this.connectedTokens.length) {
				this.connectedTokens.forEach(token => {
					transponder.socket.close(token, () => {});
				});
				this.connectedTokens = []
			}
			if (passes && passes.length && passes[0]) {
				runPass()
			}
		} else {
			running = true;
			var ch = _.clone(chain[x]);
			ch.arguments[chain[x].arguments.length - 1] = chain[x].arguments[chain[x].arguments.length - 1];
			
			scope.push(ch);
			process(x + 1)
		}
	};
	
	this.equals = function (expect, value) {
		t.equals(expect, value)
	};
	
	this.deepEquals = function (expect, value) {
		t.deepEquals(expect, value)
	};
	
	this.pass = function () {
		t.pass();
		return this
	};
	
	this.fail = function () {
		t.fail();
		return this
	};
	
	this.get = function (name, fun) {
		if (running) {
			result = FrameworkPipelineStateFuture(name, fun)
		} else {
			if (!variables.hasOwnProperty(name)) {
				throw new Error('Trying to get value of undefined variable: ' + name)
			}
			result = variables[name]
		}
		return result
	};
	
	this.getUserId = function (v) {
		if (running) {
			result = FrameworkPipelineStateFuture(v, 'getUserId')
		} else {
			var feedType = this.get('feedType') ? this.get('feedType') : 1;
			if (feedType == 4) {
				return '000000000000000000000' + (parseInt(v) + 100);
			}
			
			return v;
		}
		
		return result
	};
	
	return init()
};
