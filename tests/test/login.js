module.exports = function(p){
  p
  //register
  .sendHttp(null, 'RequestVerification', {phone: '+380930000011'}, response => {
    p.equals(response.modelName, 'RequestVerificationResp')
  })
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000011', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true)
    p.set('token', response.token)
  })
  .then()
  .connect(get('token'))
  .send(get('token'), 'User', {}, response => {
    p.equals(!!response.user, true)
    p.equals(response.deviceId, '3310')
    p.equals(response.deviceName, 'nokia')
    p.equals(!!response.user.id, true)
    p.equals(response.user.phone, '+380930000011')
  })
  //login
  .then()
  .sendHttp(null, 'RequestVerification', {phone: '+380930000011'}, function(){})
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000011', smsCode: '111', deviceId: '6200', deviceName: 'samsung', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true)
    p.set('anotherToken', response.token)
  })
  .then()
  .connect(get('anotherToken'))
  .send(get('anotherToken'), 'User', {}, response => {
    p.equals(!!response.user, true)
    p.equals(response.deviceId, '6200')
    p.equals(response.deviceName, 'samsung')
    p.equals(!!response.user.id, true)
    p.equals(response.user.phone, '+380930000011')
  })
  //update user data
  .then()
  .send(get('anotherToken'), 'UpdateUser', {user: {name: 'Some user', data: '{"foo": "bar", "baz": {"foo": ["bar", {"foo": "bar"}]}}'}}, response => {
    p.equals(response.user.name, 'Some user')
    p.deepEquals(JSON.parse(response.user.data), {"foo": "bar", "baz": {"foo": ["bar", {"foo": "bar"}]}})
  })
  .then()
  // .send(get('anotherToken'), 'User', {}, response => {
  //   p.eq_uals(response.user.name, 'Some user')
  //   p.de_epEquals(JSON.parse(response.user.data), {"foo": "bar", "baz": {"foo": ["bar", {"foo": "bar"}]}})
  // })
}
