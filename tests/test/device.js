module.exports = function(p){
  p
  //register user
  .sendHttp(null, 'RequestVerification', {phone: '+380930000011'}, response => {
    p.equals(response.modelName, 'RequestVerificationResp')
  })
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000011', smsCode: '111', os: map.Platform.ios, deviceId: '11111111', deviceName: 'nokia'}, function(response){
    p.equals(!!response.token, true)
    p.set('token', response.token)
  })
  .then()
  .connect(get('token'))
  .send(get('token'), 'User', {}, response => {
    p.equals(!!response.user, true)
    p.equals(response.os, map.Platform.ios)
    p.equals(response.deviceId, '11111111')
    p.equals(response.deviceName, 'nokia')
    p.equals(!!response.user.id, true)
    p.equals(response.user.phone, '+380930000011')
  })
  //check that now user has 1 device
  .then()
  .send(get('token'), 'Devices', {}, response => {
    p.equals(response.devices.length, 1)
    p.equals(response.devices[0].os, map.Platform.ios)
    p.equals(response.devices[0].deviceId, '11111111')
    p.equals(response.devices[0].deviceName, 'nokia')
  })
  //login with another deviceId
  .then()
  .sendHttp(null, 'RequestVerification', {phone: '+380930000011'}, response => {
    p.equals(response.modelName, 'RequestVerificationResp')
  })
  .then()
  .sendHttp(null, 'ConfirmVerification', {phone: '+380930000011', smsCode: '111', os: map.Platform.ios, deviceId: '22222222', deviceName: 'siemens'}, function(response){
    p.equals(!!response.token, true)
    p.set('anotherToken', response.token)
  })
  //check that now user has 2 different devices
  .then()
  .connect(get('anotherToken'))
  .send(get('anotherToken'), 'Devices', {}, response => {
    p.equals(response.devices.length, 2)
		const nokiaDevice = response.devices.find(device => device.deviceName === 'nokia');
		const siemensDevice = response.devices.find(device => device.deviceName === 'siemens');

    p.equals(nokiaDevice.os, map.Platform.ios)
    p.equals(nokiaDevice.deviceId, '11111111')
    p.equals(nokiaDevice.deviceName, 'nokia')

    p.equals(siemensDevice.os, map.Platform.ios)
    p.equals(siemensDevice.deviceId, '22222222')
    p.equals(siemensDevice.deviceName, 'siemens')
  })
  //logout user
  .then()
  .send(get('anotherToken'), 'Logout', {}, response => {
    p.equals(response.modelName, 'LogoutResp')
  })
  //check that token for second device is invalid
  .then()
  .sendHttp(get('anotherToken'), 'User', {}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidMessage)
  })
  //check that now user has 1 device like before
  .then()
  .send(get('token'), 'Devices', {}, response => {
    p.equals(response.devices.length, 1)
    p.equals(response.devices[0].os, map.Platform.ios)
    p.equals(response.devices[0].deviceId, '11111111')
    p.equals(response.devices[0].deviceName, 'nokia')
  })
  //add push token
  .then()
  .send(get('token'), 'AddPushToken', {type: map.PushTokenType.normal, token: 'aaaaaaaaaaa'}, response => {
    p.equals(response.modelName, 'AddPushTokenResp')
  })
  //add push token voip
  .then()
  .send(get('token'), 'AddPushToken', {type: map.PushTokenType.voip, token: 'bbbbbbbbbb'}, response => {
    p.equals(response.modelName, 'AddPushTokenResp')
  })
  .then()
  .close(get('token'))
  .close(get('anotherToken'))
}
