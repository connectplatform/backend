module.exports = function(config){
	
	this.id = config.id;
	this.ssl = config.ssl;
	this.host = config.host;
	this.protocolVersion = config.protocolVersion;
	this.debug = config.debug === true;
	
	this.getSocketUrl = (token) => {
		const protocol = this.ssl === false ? 'ws' : 'wss';
		if (this.protocolVersion) {
			return `${protocol}://${getHost()}/ws?token=${token}&version=${this.protocolVersion}`;
		} else {
			return `${protocol}://${getHost()}/ws?token=${token}`;
		}
	};

	this.getHttpUrl = (token) => {
		const protocol = this.ssl === false ? 'http' : 'https';
		if (this.protocolVersion) {
			return `${protocol}://${getHost()}/api/${this.protocolVersion}?token=${token}`;
		} else {
			return `${protocol}://${getHost()}/api?token=${token}`;
		}
	};

	this.getMapUrl = () => {
		return config.mapFile;
	};

	this.isDebug = () => {
		return this.debug;
	};
	
	const getHost = () => {
		return this.id ? `${this.id}.${this.host}` : this.host;
	};
	
	return this;
};
