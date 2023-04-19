module.exports = function(p){
  p
  .sendHttp(null, 'SocialAuth', {network: map.SocialNetwork.facebook, accessToken: '123', deviceId: '3310', deviceName: 'nokia'}, response => {
    p.equals(response.modelName, 'SocialAuthResp')
    p.equals(response.token.length > 0, true)
    p.set('loggedInUser', response.token)
  })
  .then()
  .connect(get('loggedInUser'))
  .send(get('loggedInUser'), 'User', {}, response => {
    p.equals(!!response.user, true)
    p.equals(response.deviceId, '3310')
    p.equals(response.deviceName, 'nokia')
    p.equals(!!response.user.id, true)
    p.equals(response.user.facebookId, 'facebook_user_id')
  })
  .then()
  .close(get('loggedInUser'))
  .then()
  .sendHttp(null, 'SocialAuth', {network: map.SocialNetwork.facebook, accessToken: '123', deviceId: '3310', deviceName: 'nokia'}, response => {
    p.equals(response.modelName, 'SocialAuthResp')
    p.equals(response.token.length > 0, true)
    p.set('loggedInUser2', response.token)
  })
  .then()
  .connect(get('loggedInUser2'))
  .send(get('loggedInUser2'), 'User', {}, response => {
    p.equals(!!response.user, true)
    p.equals(response.deviceId, '3310')
    p.equals(response.deviceName, 'nokia')
    p.equals(!!response.user.id, true)
    p.equals(response.user.facebookId, 'facebook_user_id')
  })
  .then()
  .close(get('loggedInUser2'))
}
