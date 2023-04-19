module.exports = function(p){
  const userId1 = '000000000000000000000001';
  const userId2 = '000000000000000000000002';
  const userId3 = '000000000000000000000003';
  p
  .set('timeBeforeContactsAdd', () => +new Date()) // Save time before contacts Add
  .then()
  // user1 add to contact user2
  .sendHttp('user1', 'AddContact', {contact: {userId: userId2}}, (response) => {
    p.equals(response.modelName, 'AddContactResp');
    p.equals(response.contact.userId, userId2);
  })
  .then()
  .sendHttp('user1', 'SyncContacts', {syncTime: get('timeBeforeContactsAdd')}, (response) => {
    p.equals(response.modelName, 'SyncContactsResp');
    p.equals(response.contacts.length, 1);
    p.equals(response.contacts[0].userId, userId2);
    p.set('timeAfterUser2ContactAdd', response.serverTime); // Save time after user1 add to contact user2
  })
  .then()
  // user1 add to contact user3
  .sendHttp('user1', 'AddContact', {contact: {userId: userId3}}, (response) => {
    p.equals(response.modelName, 'AddContactResp');
    p.equals(response.contact.userId, userId3);
  })
  .then()
  .sendHttp('user1', 'SyncContacts', {syncTime: get('timeAfterUser2ContactAdd')}, (response) => {
    p.equals(response.modelName, 'SyncContactsResp');
    p.equals(response.contacts.length, 1);
    p.equals(response.contacts[0].userId, userId3);
    p.set('timeAfterUser3ContactAdd', response.serverTime); // Save time after user1 add to contact user3
  })
  .then()
  .sendHttp('user1', 'SyncContacts', {syncTime: get('timeAfterUser3ContactAdd')}, (response) => {
    p.equals(response.modelName, 'SyncContactsResp');
    p.equals(response.contacts.length, 0);
  })
  .sendHttp('user1', 'SyncContacts', {syncTime: get('timeBeforeContactsAdd')}, (response) => {
    p.equals(response.modelName, 'SyncContactsResp');
    p.equals(response.contacts.length, 2);
    p.equals((response.contacts.find(u => u.userId === userId2)).userId, userId2);
    p.equals((response.contacts.find(u => u.userId === userId3)).userId, userId3);
    p.set('timeBeforeUser2Blocked', response.serverTime);
  })
  .then()
  .sendHttp('user1', 'SyncContacts', {syncTime: get('timeBeforeUser2Blocked')}, (response) => {
    p.equals(response.modelName, 'SyncContactsResp');
    p.equals(response.contacts.length, 0);
  })
  .then()
  .sendHttp('user1', 'BlockContact', {userId: userId2}, (response) => {
    p.equals(response.modelName, 'BlockContactResp');
    p.equals(response.contact.userId, userId2);
  })
  .then()
  .sendHttp('user1', 'SyncContacts', {syncTime: get('timeBeforeUser2Blocked')}, (response) => {
    p.equals(response.modelName, 'SyncContactsResp');
    p.equals(response.contacts.length, 1);
    p.equals(response.contacts[0].userId, userId2);
    p.set('timeBeforeUser2UnBlocked', response.serverTime);
  })
  .then()
  .sendHttp('user1', 'SyncContacts', {syncTime: get('timeBeforeUser2UnBlocked')}, (response) => {
    p.equals(response.modelName, 'SyncContactsResp');
    p.equals(response.contacts.length, 0);
  })
  .then()
  .sendHttp('user1', 'UnBlockContact', {userId: userId2}, (response) => {
    p.equals(response.modelName, 'UnBlockContactResp');
    p.equals(response.contact.userId, userId2);
  })
  .then()
  .sendHttp('user1', 'SyncContacts', {syncTime: get('timeBeforeUser2UnBlocked')}, (response) => {
    p.equals(response.modelName, 'SyncContactsResp');
    p.equals(response.contacts.length, 1);
    p.equals(response.contacts[0].userId, userId2);
  })
}
