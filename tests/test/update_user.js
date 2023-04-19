module.exports = function(p){
  p
  .sendHttp(null, 'RequestVerification', {phone: '+380930000011'}, function(){})
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000011', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true)
    p.set('token', response.token)
  })
  .then()
  .connect(get('token'))
  .send(get('token'), 'UpdateUser', {user: {name: 'John', photo: 'http://avatar.ru/1425.jpg', thumbnail: 'http://avatar.ru/1425_small.jpg', data: "{\"foo\":\"bar\",\"bar\":\"baz\"}", excludeMe: true}}, function(response){
    p.equals(response.user.name, 'John')
    p.equals(response.user.excludeMe, true)
    p.equals(response.user.photo, 'http://avatar.ru/1425.jpg')
    p.equals(response.user.thumbnail, 'http://avatar.ru/1425_small.jpg')
  })
  .then()
  .send(get('token'), 'User', {}, function(response){
    p.equals(response.user.name, 'John')
    p.equals(response.user.excludeMe, true)
    p.equals(response.user.photo, 'http://avatar.ru/1425.jpg')
    p.equals(response.user.thumbnail, 'http://avatar.ru/1425_small.jpg')
    p.deepEquals(JSON.parse(response.user.data), {"foo": "bar", "bar": "baz"})
  })
}
