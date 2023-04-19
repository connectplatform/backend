const fs = require('fs');

module.exports = function() {
	const configFilePath = '../apps/im/priv/data/app_conf.json';
	
	return {
		updateFile: (content) => {
			fs.writeFile(configFilePath, JSON.stringify(content), (err) => {
				console.log('File updated!');
				if (err) {
					throw err;
				}
			})
		},
	}
};
