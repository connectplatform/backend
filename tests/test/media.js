module.exports = function(p){
  p
  .connect('user1')
	.then()
	.send('user1', 'Message', {
    feedType: map.MessageFeedType.chat,
    feedId: '000000000000000000000001',
    message: {
      kind: map.MessageKind.text,
      payload: 'Message 1',
      media: [
        {
          type: map.MediaType.link,
          link: 'https://www.facebook.com/emotionesthetique'
        }
      ]
    }
  }, response => {
		p.equals(response.modelName, 'MessageResp');
    p.equals(response.message.payload, 'Message 1');
    p.equals(response.message.media.length, 1);
    p.equals(response.message.media[0].type, map.MediaType.link);
    p.equals(response.message.media[0].link, 'https://www.facebook.com/emotionesthetique');
  })
  .then()
	.send('user1', 'FetchOgData', {url: 'https://www.facebook.com/emotionesthetique'}, response => {
    p.equals(response.modelName, 'FetchOgDataResp');
    p.set('fetched', response.ogData.fetched);
  })
  .then()
	.send('user1', 'FetchOgData', {url: 'https://www.facebook.com/emotionesthetique'}, response => {
    p.equals(response.modelName, 'FetchOgDataResp');
    p.equals(response.ogData.fetched, get('fetched'));
  })
};
