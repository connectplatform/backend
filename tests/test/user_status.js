module.exports = function(p){
  p
  .connect('user1')
  .sendHttp('user1', 'AddContact', {contact: {userId: '000000000000000000000002'}}, (response) => {
    p.equals(response.modelName, 'AddContactResp')
    p.equals(response.contact.userId, '000000000000000000000002')
  })
  .then()
  .sendHttp('user2', 'AddContact', {contact: {userId: '000000000000000000000001'}}, (response) => {
    p.equals(response.modelName, 'AddContactResp')
    p.equals(response.contact.userId, '000000000000000000000001')
  })
  //check that user1 received UserStatusChanged when user2 goes online
  .then()
  .wait('user1', {name: 'UserStatusChanged', priority: true}, response => {
    p.equals(response.status.userId, '000000000000000000000002')
    p.equals(response.status.lastSeen, null)
    p.equals(response.status.status, map.UserStatus.online)
  })
  .connect('user2')
  //check Status
  .then()
  .sendHttp('user1', 'Status', {userId: '000000000000000000000002'}, response => {
    p.equals(response.status.userId, '000000000000000000000002')
    p.equals(response.status.lastSeen, null)
    p.equals(response.status.status, map.UserStatus.online)
  })
  .then()
  .sendHttp('user2', 'Status', {userId: '000000000000000000000001'}, response => {
    p.equals(response.status.userId, '000000000000000000000001')
    p.equals(response.status.lastSeen, null)
    p.equals(response.status.status, map.UserStatus.online)
  })
  //check Statuses
  .then()
  .sendHttp('user1', 'Statuses', {}, response => {
    p.equals(response.statuses.length, 1)
    p.equals(response.statuses[0].userId, '000000000000000000000002')
    p.equals(response.statuses[0].lastSeen, null)
    p.equals(response.statuses[0].status, map.UserStatus.online)
  })
  .then()
  .sendHttp('user2', 'Statuses', {}, response => {
    p.equals(response.statuses.length, 1)
    p.equals(response.statuses[0].userId, '000000000000000000000001')
    p.equals(response.statuses[0].lastSeen, null)
    p.equals(response.statuses[0].status, map.UserStatus.online)
  })
  //check Status after user2 goes offline
  .then()
  .close('user2')
  .and()
  .timeout(100)
  .and()
  .sendHttp('user1', 'Status', {userId: '000000000000000000000002'}, response => {
    p.equals(response.status.userId, '000000000000000000000002')
    p.equals(!!response.status.lastSeen, true)
    p.equals(response.status.status, map.UserStatus.offline)
  })
}
