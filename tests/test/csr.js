module.exports = function(p){

	const buttonCloseRequestText = 'Close ticket';
	const buttonDropRequestText = 'Drop ticket';

  p
  .connect('user1')
  .connect('user2')
  .connect('user3')
  .then()
  .send('user1', 'GetCircle', {supervisor: 'testbot', params: 'action=buyCar&carId=22&topic=Topic1&thumbnail=image.jpeg', members: ['000000000000000000000003']}, response => {
    p.equals(response.modelName, 'GetCircleResp');
    p.set('roomId1', response.roomId)
  })

  //check create request
  .then()
  .send('user1', 'CreateCsr', {request: {
    roomId: get('roomId1'),
    userId: '000000000000000000000003' // Create request from user3
  }}, response => {
    p.set('requestId1', response.request.id);
    p.set('roomId1', response.request.roomId);
    p.equals(response.modelName, 'CreateCsrResp');
    p.equals(response.request.userId, '000000000000000000000003');
    p.equals(response.request.status, map.CsrStatus.open);
    p.equals(response.request.descr, 'Topic1');
    // p.e_quals(response.request.thumbnail, 'image.jpeg');
    p.deepEquals(response.request.tags, [])
  })
	.wait('user3', 'ChatUpdate', response => {
		const replyKeyboardMarkup = response.update.replyKeyboardMarkup;
		p.equals(!!replyKeyboardMarkup, true);
		p.equals(replyKeyboardMarkup.modelName, 'ReplyKeyboardMarkupEntity');
    p.equals(replyKeyboardMarkup.buttonRows.length, 1);
    p.equals(replyKeyboardMarkup.buttonRows[0].buttons.length, 1);
		p.equals(replyKeyboardMarkup.buttonRows[0].buttons[0].modelName, 'KeyboardButtonEntity');
		p.equals(replyKeyboardMarkup.buttonRows[0].buttons[0].text, buttonCloseRequestText);
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => {
	  p.equals(response.modelName, 'ChatUpdatesResp');
	  p.equals(response.updates.length, 1);
	  const replyKeyboardMarkup = response.updates[0].replyKeyboardMarkup;
	  p.equals(!!replyKeyboardMarkup, true);
	  p.equals(replyKeyboardMarkup.modelName, 'ReplyKeyboardMarkupEntity');
	  p.equals(replyKeyboardMarkup.buttonRows.length, 1);
	  p.equals(replyKeyboardMarkup.buttonRows[0].buttons.length, 1);
	  p.equals(replyKeyboardMarkup.buttonRows[0].buttons[0].modelName, 'KeyboardButtonEntity');
	  p.equals(replyKeyboardMarkup.buttonRows[0].buttons[0].text, buttonCloseRequestText);
  })

  //check single open request per room restriction
  .then()
  .send('user1', 'CreateCsr', {request: {
    roomId: get('roomId1'),
    userId: '000000000000000000000003', // Create request from user3
    descr: 'Topic1'
  }}, response => {
    p.equals(response.modelName, 'ErrorResp');
  })

  //check add/remove tag
  .then()
  .send('user1', 'AddTagCsr', {id: get('requestId1'), tag: 'Tag 1'}, response => {
    p.equals(response.modelName, 'AddTagCsrResp');
    p.deepEquals(response.request.tags, ['Tag 1'])
  })
  .then()
  .send('user1', 'AddTagCsr', {id: get('requestId1'), tag: 'Tag 2'}, response => {
    p.equals(response.modelName, 'AddTagCsrResp')
  })
  .then()
  .send('user1', 'AddTagCsr', {id: get('requestId1'), tag: 'Tag 3'}, response => {
    p.equals(response.modelName, 'AddTagCsrResp')
  })
  .then()
  .send('user1', 'RemoveTagCsr', {id: get('requestId1'), tag: 'Tag 2'}, response => {
    p.equals(response.modelName, 'RemoveTagCsrResp')
  })
  .then()
  .send('user1', 'OpenCsr', {count: -1}, response => {
    p.equals(response.modelName, 'OpenCsrResp');
    p.equals(response.requests.length, 1);
    p.equals(response.requests[0].id, get('requestId1'));
    p.deepEquals(response.requests[0].tags, ['Tag 1', 'Tag 3'])
  })

  //check that new request is in listing now
  .then()
  .send('user1', 'OpenCsr', {count: -1}, response => {
    p.equals(response.modelName, 'OpenCsrResp');
    p.equals(response.requests.length, 1);
    p.equals(response.requests[0].id, get('requestId1'));
    p.equals(response.requests[0].userId, '000000000000000000000003');
    p.equals(response.requests[0].status, map.CsrStatus.open);
    p.equals(response.requests[0].descr, 'Topic1')
  })
  .then()
  .send('user2', 'OpenCsr', {count: -1}, response => {
    p.equals(response.modelName, 'OpenCsrResp');
    p.equals(response.requests[0].id, get('requestId1'))
  })

  //check forbidden actions
  .then()
  .send('user1', 'DropCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })
  .then()
  .send('user1', 'CloseCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })

  //check grab request
  .then()
  .send('user1', 'GrabCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'GrabCsrResp');
    p.equals(response.request.id, get('requestId1'));
    p.equals(response.request.userId, '000000000000000000000003');
    p.equals(response.request.status, map.CsrStatus.grabbed);
    p.equals(response.request.descr, 'Topic1')
  })
  .wait('user1', 'ChatUpdate', response => {
	  const replyKeyboardMarkup = response.update.replyKeyboardMarkup;
	  p.equals(!!replyKeyboardMarkup, true);
	  p.equals(replyKeyboardMarkup.modelName, 'ReplyKeyboardMarkupEntity');
	  p.equals(replyKeyboardMarkup.buttonRows.length, 1);
	  p.equals(replyKeyboardMarkup.buttonRows[0].buttons.length, 2);
	  p.equals(!!replyKeyboardMarkup.buttonRows[0].buttons.find(button => button.text === buttonDropRequestText), true);
	  p.equals(!!replyKeyboardMarkup.buttonRows[0].buttons.find(button => button.text === buttonCloseRequestText), true);
  })
  .then()
  .send('user1', 'ChatUpdates', {}, response => {
	  p.equals(response.modelName, 'ChatUpdatesResp');
	  p.equals(response.updates.length, 1);
	  const replyKeyboardMarkup = response.updates[0].replyKeyboardMarkup;
	  p.equals(!!replyKeyboardMarkup, true);
	  p.equals(replyKeyboardMarkup.modelName, 'ReplyKeyboardMarkupEntity');
	  p.equals(replyKeyboardMarkup.buttonRows.length, 1);
	  p.equals(replyKeyboardMarkup.buttonRows[0].buttons.length, 2);
	  p.equals(!!replyKeyboardMarkup.buttonRows[0].buttons.find(button => button.text === buttonDropRequestText), true);
	  p.equals(!!replyKeyboardMarkup.buttonRows[0].buttons.find(button => button.text === buttonCloseRequestText), true);
  })

  //check that grabbed request is NOT in listing now
  .then()
  .send('user1', 'OpenCsr', {count: -1}, response => {
    p.equals(response.modelName, 'OpenCsrResp');
    p.equals(response.requests.length, 0)
  })

  //check that user can't grab already grabbed request
  .then()
  .send('user1', 'GrabCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.invalidMessage)
  })

  //check that other user can't do anyting with grabbed request
  .then()
  .send('user2', 'GrabCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.invalidMessage)
  })
  .then()
  .send('user2', 'DropCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })
  .then()
  .send('user2', 'CloseCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })

  //check drop request
  .then()
  .send('user1', 'DropCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'DropCsrResp');
    p.equals(response.request.id, get('requestId1'));
    p.equals(response.request.userId, '000000000000000000000003');
    p.equals(response.request.status, map.CsrStatus.dropped);
    p.equals(response.request.descr, 'Topic1')
  })
  .wait('user1', 'ChatUpdate', response => {
	  p.equals(!response.update.replyKeyboardMarkup, true);
  })
  .then()
  .send('user1', 'ChatUpdates', {}, response => {
	  p.equals(response.modelName, 'ChatUpdatesResp');
	  p.equals(response.updates.length, 1);
	  p.equals(!response.updates[0].replyKeyboardMarkup, true);
  })

  //check that request is in listing again
  .then()
  .send('user3', 'OpenCsr', {count: -1}, response => {
    p.equals(response.modelName, 'OpenCsrResp');
    p.equals(response.requests.length, 1);
    p.equals(response.requests[0].id, get('requestId1'));
    p.equals(response.requests[0].userId, '000000000000000000000003');
    p.equals(response.requests[0].status, map.CsrStatus.dropped);
    p.equals(response.requests[0].descr, 'Topic1')
  })

  //check close request
  .then()
  .send('user2', 'GrabCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'GrabCsrResp')
  })
  .then()
  .send('user2', 'ChatUpdates', {}, response => { // check that grabber have close and drop buttons
	  p.equals(response.modelName, 'ChatUpdatesResp');
	  p.equals(response.updates.length, 1);
	  const replyKeyboardMarkup = response.updates[0].replyKeyboardMarkup;
	  p.equals(!!replyKeyboardMarkup, true);
	  p.equals(replyKeyboardMarkup.modelName, 'ReplyKeyboardMarkupEntity');
	  p.equals(replyKeyboardMarkup.buttonRows.length, 1);
	  p.equals(replyKeyboardMarkup.buttonRows[0].buttons.length, 2);
	  p.equals(!!replyKeyboardMarkup.buttonRows[0].buttons.find(button => button.text === buttonDropRequestText), true);
	  p.equals(!!replyKeyboardMarkup.buttonRows[0].buttons.find(button => button.text === buttonCloseRequestText), true);
  })
  .send('user3', 'ChatUpdates', {}, response => { // check that creator have close request button
	  p.equals(response.modelName, 'ChatUpdatesResp');
	  p.equals(response.updates.length, 1);
	  const replyKeyboardMarkup = response.updates[0].replyKeyboardMarkup;
	  p.equals(!!replyKeyboardMarkup, true);
	  p.equals(replyKeyboardMarkup.modelName, 'ReplyKeyboardMarkupEntity');
	  p.equals(!!replyKeyboardMarkup.buttonRows, true);
    p.equals(replyKeyboardMarkup.buttonRows.length, 1);
    p.equals(replyKeyboardMarkup.buttonRows[0].buttons.length, 1);
	  p.equals(replyKeyboardMarkup.buttonRows[0].buttons[0].modelName, 'KeyboardButtonEntity');
	  p.equals(replyKeyboardMarkup.buttonRows[0].buttons[0].text, buttonCloseRequestText);
  })
  .then()
  .send('user2', 'CloseCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'CloseCsrResp');
    p.equals(response.request.id, get('requestId1'));
    p.equals(response.request.userId, '000000000000000000000003');
    p.equals(response.request.status, map.CsrStatus.closed);
    p.equals(response.request.descr, 'Topic1');

    p.deepEquals(response.request.staff, ['000000000000000000000001', '000000000000000000000002']);

    p.deepEquals(response.request.activity.length, 5);
    p.deepEquals(response.request.activity[0].action, 'create');
    p.deepEquals(response.request.activity[0].userId, '000000000000000000000001');
    p.deepEquals(response.request.activity[1].action, 'grab');
    p.deepEquals(response.request.activity[1].userId, '000000000000000000000001');
    p.deepEquals(response.request.activity[2].action, 'drop');
    p.deepEquals(response.request.activity[2].userId, '000000000000000000000001');
    p.deepEquals(response.request.activity[3].action, 'grab');
    p.deepEquals(response.request.activity[3].userId, '000000000000000000000002');
    p.deepEquals(response.request.activity[4].action, 'close');
    p.deepEquals(response.request.activity[4].userId, '000000000000000000000002')
  })
  .wait('user3', 'ChatUpdate', response => {
	  p.equals(response.update.replyKeyboardMarkup, null);
  })
  .wait('user2', 'ChatUpdate', response => {
	  p.equals(response.update.replyKeyboardMarkup, null);
  })
  .then()
  .send('user3', 'ChatUpdates', {}, response => { // check that creator haven't close request button
	  p.equals(response.modelName, 'ChatUpdatesResp');
	  p.equals(response.updates.length, 1);
	  p.equals(response.updates[0].replyKeyboardMarkup, null);
  })
  .send('user2', 'ChatUpdates', {}, response => { // check that grabber haven't close or drop buttons
	  p.equals(response.modelName, 'ChatUpdatesResp');
	  p.equals(response.updates.length, 1);
	  p.equals(response.updates[0].replyKeyboardMarkup, null);
  })

  //check forbidden actions
  .then()
  .send('user1', 'GrabCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.invalidMessage)
  })
  .then()
  .send('user1', 'DropCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })
  .then()
  .send('user1', 'CloseCsr', {id: get('requestId1')}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })

  //create customer user
  .then()
  .sendHttp(null, 'RequestVerification', {phone: '+380930000033'}, function(){})
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000033', smsCode: '111', deviceId: '3310', deviceName: 'nokia', os: map.Platform.ios}, function(response){
    p.equals(!!response.token, true);
    p.set('customerUser', response.token)
  })
  .then()
  .connect(get('customerUser'))
  .send(get('customerUser'), 'UpdateUser', {user: {name: 'customerUser'}}, function(response){
    p.equals(response.user.name, 'customerUser');
    p.set('customerUserId', response.user.id)
  })
  .then()
  .send('user1', 'UpdateUser', {user: {id: get('customerUserId'), name: 'customerUser', active: true, vendorId: get('customerUserId'), isVendor: false, roles: ['customer']}}, response => {
    p.equals(response.user.id, get('customerUserId'));
    p.deepEquals(response.user.roles, ['customer'])
  })
  //check that customer user can't manage requests
  .then()
  .send(get('customerUser'), 'OpenCsr', {count: -1}, response => {
    p.equals(response.modelName, 'ErrorResp');
    p.equals(response.code, map.ErrorCode.permissionDenied)
  })
};
