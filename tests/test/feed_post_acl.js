module.exports = function(p){
  p
  .connect('user1')
  //check that super_admin can create ticket
  .send('user1', 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 1'}}, response => {
    p.equals(response.modelName, 'FeedPostCreateResp')
  })
  //create vendorUser
  .then()
  .sendHttp(null, 'RequestVerification', {phone: '+380930000011'}, function(){})
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000011', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true)
    p.set('vendorUser', response.token)
  })
  .then()
  .connect(get('vendorUser'))
  .send(get('vendorUser'), 'UpdateUser', {user: {name: 'vendorUser'}}, function(response){
    p.equals(response.user.name, 'vendorUser')
    p.set('vendorUserId', response.user.id)
  })
  .then()
  .send('user1', 'UpdateUser', {user: {id: get('vendorUserId'), name: 'vendorUser', active: true, isVendor: true, roles: ['vendor']}}, response => {
    p.equals(response.user.id, get('vendorUserId'))
    p.deepEquals(response.user.roles, ['vendor'])
  })
  //create vendorUser2
  .then()
  .sendHttp(null, 'RequestVerification', {phone: '+380930000044'}, function(){})
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000044', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true)
    p.set('vendorUser2', response.token)
  })
  .then()
  .connect(get('vendorUser2'))
  .send(get('vendorUser2'), 'UpdateUser', {user: {name: 'vendorUser2'}}, function(response){
    p.equals(response.user.name, 'vendorUser2')
    p.set('vendorUserId2', response.user.id)
  })
  //> we will assign him vendor role after test
  //create vendorStaffUser
  .then()
  .sendHttp(null, 'RequestVerification', {phone: '+380930000022'}, function(){})
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000022', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true)
    p.set('vendorStaffUser', response.token)
  })
  .then()
  .connect(get('vendorStaffUser'))
  .send(get('vendorStaffUser'), 'UpdateUser', {user: {name: 'vendorStaffUser'}}, function(response){
    p.equals(response.user.name, 'vendorStaffUser')
    p.set('vendorStaffUserId', response.user.id)
  })
  .then()
  .connect(get('vendorStaffUser'))
  .send('user1', 'UpdateUser', {user: {id: get('vendorStaffUserId'), name: 'vendorStaffUser', active: true, vendorId: get('vendorUserId'), isVendor: false, roles: ['vendor_staff']}}, response => {
    p.equals(response.user.id, get('vendorStaffUserId'))
    p.deepEquals(response.user.roles, ['vendor_staff'])
  })
  //create vendorRepUser
  .then()
  .sendHttp(null, 'RequestVerification', {phone: '+380930000033'}, function(){})
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000033', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true)
    p.set('vendorRepUser', response.token)
  })
  .then()
  .connect(get('vendorRepUser'))
  .send(get('vendorRepUser'), 'UpdateUser', {user: {name: 'vendorRepUser'}}, function(response){
    p.equals(response.user.name, 'vendorRepUser')
    p.set('vendorRepUserId', response.user.id)
  })
  .then()
  .send('user1', 'UpdateUser', {user: {id: get('vendorRepUserId'), name: 'vendorRepUser', active: true, vendorId: get('vendorUserId'), isVendor: false, roles: ['vendor_rep']}}, response => {
    p.equals(response.user.id, get('vendorRepUserId'))
    p.deepEquals(response.user.roles, ['vendor_rep'])
  })
  //check that customer cant create ticket
  .then()
  .send(get('vendorUser2'), 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 1'}}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })
  //check vendor functions
  .then()
  .send('user1', 'UpdateUser', {user: {id: get('vendorUserId2'), name: 'vendorUser2', active: true, isVendor: true, roles: ['vendor']}}, response => {
    p.equals(response.user.id, get('vendorUserId2'))
    p.deepEquals(response.user.roles, ['vendor'])
  })
  .then()
  .send(get('vendorUser'), 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 1'}}, response => {
    p.set('feedPostId1', response.post.id)
    p.equals(response.post.title, 'Ticket 1')
    p.equals(response.post.vendorId, get('vendorUserId'))
  })
  .then()
  .send(get('vendorUser'), 'FeedPostUpdate', {post: {id: get('feedPostId1'), type: 'tickets', title: 'Ticket 11'}}, response => {
    p.equals(response.post.title, 'Ticket 11')
    p.equals(response.post.vendorId, get('vendorUserId'))
  })
  .then()
  .send(get('vendorUser'), 'FeedPostDelete', {id: get('feedPostId1')}, response => {
    p.equals(response.modelName, 'FeedPostDeleteResp')
  })
  //check vendor staff functions
  .then()
  .send(get('vendorStaffUser'), 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 2'}}, response => {
    p.set('feedPostId2', response.post.id)
    p.equals(response.post.title, 'Ticket 2')
    p.equals(response.post.vendorId, get('vendorUserId'))
  })
  //check vendorsOnly flag
  .then()
  .send(get('vendorUser2'), 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 3'}}, response => {
    p.set('feedPostId3', response.post.id)
    p.equals(response.post.title, 'Ticket 3')
    p.equals(response.post.vendorId, get('vendorUserId2'))
  })
  .then()
  .send(get('vendorUser2'), 'FeedPost', {type: 'tickets', vendorsOnly: true}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].id, get('feedPostId3'))
  })
  .then()
  .send(get('vendorStaffUser'), 'FeedPost', {type: 'tickets', vendorsOnly: true}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].id, get('feedPostId2'))
  })
  //check get by vendorId
  .then()
  .send(get('vendorUser2'), 'FeedPost', {type: 'tickets', vendorId: get('vendorUserId2')}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].id, get('feedPostId3'))
  })
  .then()
  .send(get('vendorUser'), 'FeedPost', {type: 'tickets', vendorId: get('vendorUserId')}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].id, get('feedPostId2'))
  })
}
