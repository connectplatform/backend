module.exports = function(p){
	const message = {
		ref: 100,
		media: {
			chunk: 0,
			chunkSize: 0,
			duration: 0,
			height: 0,
			media: new Uint8Array([137,80,78,71,13,10,26,10,0,0,0,13,73,72,68,82,0,0,0,156,0,0,0,]),
			mime: 'png',
			name: '1.png',
			totalChunks: 0,
			totalSize: 0,
			type: 1,
			width: 0
		}
	};
	
  p
  .connect('user1')
  .send('user1', 'UploadMedia', message, response => {
	  p.equals(response.modelName, 'UploadMediaResp');
	  p.equals(!!response.url, true);
  })
  .then()
  .sendHttp('user1', 'UploadMedia', message, response => {
	  p.equals(response.modelName, 'UploadMediaResp');
	  p.equals(!!response.url, true);
  })
};
