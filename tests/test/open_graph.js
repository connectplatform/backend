module.exports = function(p){
  p
  .connect('user1')
  //check fetch
  .send('user1', 'FetchOgData', {url: 'http://example.com'}, response => {
    p.equals(response.modelName, 'FetchOgDataResp')
    p.equals(response.ogData.url, 'http://example.com')
    p.equals(response.ogData.favicon, 'http://example.com/favicon.ico')
    p.equals(response.ogData.domain, 'example.com')
    p.equals(response.ogData.title, 'Example title')
    p.equals(response.ogData.description, 'Example description')
    p.equals(response.ogData.image, 'http://example.com/image.jpg')
    p.equals(response.ogData.context, 'Example context')
    p.equals(response.ogData.fetched > 0, true)

    p.equals(response.ogData.media.length, 1)
    p.equals(response.ogData.media[0].image, 'http://example/media/0/image.jpg')
    p.equals(response.ogData.media[0].thumbnail, 'http://example/media/0/thumbnail.jpg')
    p.equals(response.ogData.media[0].description, 'Media description')
  })
  //check fetch
  .then()
  .send('user1', 'FetchOgData', {url: 'http://example.com?refetch=1'}, response => {
    p.equals(response.modelName, 'FetchOgDataResp')
    p.equals(response.ogData.url, 'http://example.com')
    p.equals(response.ogData.favicon, 'http://example.com/favicon.ico')
    p.equals(response.ogData.domain, 'example.com')
    p.equals(response.ogData.title, 'Example title refetched')
    p.equals(response.ogData.description, 'Example description')
    p.equals(response.ogData.image, 'http://example.com/image.jpg')
    p.equals(response.ogData.context, 'Example context')
  })
}
