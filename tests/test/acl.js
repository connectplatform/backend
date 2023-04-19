module.exports = function(p){
  p
  .connect('user1')
  .connect('user2')
  //check that super_admin can create ticket
  .send('user1', 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 1'}}, response => {
    p.equals(response.modelName, 'FeedPostCreateResp')
  })
  //check that customer cant create ticket
  .then()
  .sendHttp(null, 'RequestVerification', {phone: '+380930000011'}, function(){})
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000011', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true)
    p.set('newUser', response.token)
  })
  .then()
  .connect(get('newUser'))
  .send(get('newUser'), 'UpdateUser', {user: {name: 'NewUser'}}, function(response){
    p.equals(response.user.name, 'NewUser')
    p.set('newUserId', response.user.id)
  })
  .then()
  .send(get('newUser'), 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 1'}}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })
  //check that customer can update own profile
  .then()
  .send(get('newUser'), 'UpdateUser', {user: {name: 'Rubick'}}, response => {
    p.equals(response.modelName, 'UserResp')
  })
  //check that customer can't update other user profile
  .then()
  .send(get('newUser'), 'UpdateUser', {user: {id: '000000000000000000000001', name: 'Rubick5'}}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })
  //check get user roles
  .then()
  send('user1', 'UserRoles', {}, response => {
    p.equals(response.modelName, 'UserRolesResp')
  })
}
