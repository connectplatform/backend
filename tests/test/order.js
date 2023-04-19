module.exports = function(p){
  p
  .connect('user1')
  .connect('user2')
  .send('user1', 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 1', paymentInfo: {type: map.FeedPostPaymentType.ticket, price: 2000, currency: 'EUR'}}}, response => {
    p.set('productId', response.post.id)
    p.equals(response.post.title, 'Ticket 1')
  })
  //create order1
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 5}, response => {
    p.set('orderId1', response.order.id)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 5)
    p.equals(response.order.total, 10000)
  })
  .and()
  .wait('user1', 'OrderChanged', response => {
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 5)
  })
  //check get orders by ids
  .then()
  .send('user1', 'Orders', {orderIds: [get('orderId1')]}, response => {
    p.equals(response.orders.length, 1)
    p.equals(response.orders[0].id, get('orderId1'))
  })
  //charge order
  .then()
  .send('user1', 'ChargeOrder', {orderIds: [get('orderId1')]}, response => {
    p.equals(response.orders.length, 1)
  })
  .then()
  .send('user1', 'Orders', {status: 1/*pending*/}, response => {
    p.equals(response.orders.length, 0);
  })
  .then()
  .send('user1', 'Orders', {status: 2/*paid*/}, response => {
    p.equals(response.orders.length, 1);
    p.equals(response.orders[0].id, get('orderId1'));
  })
  //create order2
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 10}, response => {
    p.set('orderId2', response.order.id)
  })
  .then()
  .send('user1', 'Orders', {}, response => {
    p.equals(response.orders.length, 2);

    p.equals(response.orders[0].qty, 5);
    p.equals(response.orders[0].price, 2000);
    p.equals(response.orders[0].total, 10000);

    p.equals(response.orders[1].qty, 10);
    p.equals(response.orders[1].price, 2000);
    p.equals(response.orders[1].total, 20000);
  })
  //delete order1
  .then()
  .send('user1', 'DeleteOrder', {orderId: get('orderId1')}, response => {
    p.pass()
  })
  .then()
  .send('user1', 'Orders', {}, response => {
    p.equals(response.orders.length, 1);
    p.equals(response.orders[0].id, get('orderId2'));
  })
  //check merging pending orders
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 10}, response => {
    p.equals(response.order.id, get('orderId2'))
    p.equals(response.order.qty, 20)
    p.equals(response.order.total, 40000)
  })
  .and()
  .wait('user1', 'OrderChanged', response => {
    p.equals(response.order.id, get('orderId2'))
  })
  .then()
  .send('user1', 'UseOrder', {serial: 'wrong', qty: 1}, response => {
    p.equals(response.modelName, 'ErrorResp')
  })
  //check merging paid orders
  .then()
  .send('user1', 'ChargeOrder', {orderIds: [get('orderId2')]}, response => {
    p.equals(response.orders.length, 1)
    p.set('orderSerial2', response.orders[0].serial)
  })
  .and()
  .wait('user1', 'OrderChanged', response => {
    p.equals(response.order.id, get('orderId2'))
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 10}, response => {
    p.set('orderId3', response.order.id)
  })
  .and()
  .wait('user1', 'OrderChanged', response => {
    p.equals(response.order.qty, 10)
  })
  .then()
  .send('user1', 'ChargeOrder', {orderIds: [get('orderId3')]}, response => {
    p.equals(response.orders.length, 1)
    p.equals(response.orders[0].id, get('orderId2'))
    p.equals(response.orders[0].qty, 30)
  })
  .and()
  .wait('user1', 'OrderDeleted', response => {
    p.equals(response.orderId, get('orderId3'))
  })
  .and()
  .wait('user1', 'OrderChanged', response => {
    p.equals(response.order.id, get('orderId2'))
    p.equals(response.order.qty, 30)
  })
  //check find order by id and serial
  .then()
  .send('user1', 'Order', {orderId: get('orderId2')}, response => {
    p.equals(response.order.id, get('orderId2'))
  })
  .then()
  .send('user1', 'Order', {serial: get('orderSerial2')}, response => {
    p.equals(response.order.id, get('orderId2'))
  })
  //check using order2
  .then()
  .send('user1', 'UseOrder', {serial: get('orderSerial2'), qty: 1}, response => {
    p.equals(response.order.availableUseQty, 29)
  })
  .then()
  .send('user1', 'UseOrder', {serial: get('orderSerial2'), qty: 29}, response => {
    p.equals(response.order.availableUseQty, 0)
  })
  .then()
  .send('user1', 'UseOrder', {serial: get('orderSerial2'), qty: 1}, response => {
    p.equals(response.modelName, 'ErrorResp')
  })
  .then()
  .send('user1', 'Orders', {status: map.OrderStatus.archieved}, response => {
    p.equals(response.orders.length, 1)
    p.equals(response.orders[0].id, get('orderId2'))
  })
  //change order
  .then()
  .send('user2', 'CreateOrder', {productId: get('productId'), qty: 10}, response => {
    p.set('orderId3', response.order.id)
    p.equals(response.order.qty, 10)
    p.equals(response.order.total, 20000)
  })
  .and()
  .wait('user2', 'OrderChanged', response => {
    p.equals(response.order.id, get('orderId3'))
    p.equals(response.order.qty, 10)
    p.equals(response.order.total, 20000)
  })
  .then()
  .send('user2', 'ChangeOrder', {order: {id: get('orderId3'), qty: 5}}, response => {
    p.equals(response.order.qty, 5)
    p.equals(response.order.total, 10000)
  })
  .and()
  .wait('user2', 'OrderChanged', response => {
    p.equals(response.order.id, get('orderId3'))
    p.equals(response.order.qty, 5)
    p.equals(response.order.total, 10000)
  })
  //check vendorsOnly flag
  //> create vendorUser
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
  // >create product, order, and check vendorsOnly
  .then()
  .send(get('vendorUser'), 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 2', paymentInfo: {type: map.FeedPostPaymentType.ticket, price: 2000, currency: 'EUR', useQty: 10}}}, response => {
    p.set('productId2', response.post.id)
    p.equals(response.post.title, 'Ticket 2')
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId2'), qty: 10}, response => {
    p.set('orderId4', response.order.id)
    p.equals(response.order.qty, 10)
    p.equals(response.order.total, 20000)
  })
  .then()
  .send(get('vendorUser'), 'Orders', {vendorsOnly: true}, response => {
    p.equals(response.orders.length, 1)
    p.equals(response.orders[0].id, get('orderId4'))
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 10}, response => {
    p.set('orderId5', response.order.id)
    p.equals(response.order.qty, 10)
    p.equals(response.order.total, 20000)
  })
  .then()
  .send('user1', 'Orders', {status: map.OrderStatus.pending}, response => {
    p.equals(response.orders.length, 2)
    p.equals(response.orders[0].id, get('orderId4'))
    p.equals(response.orders[1].id, get('orderId5'))
  })
  .then()
  .send('user1', 'ChargeOrder', {orderIds: [get('orderId4'), get('orderId5')]}, response => {
    p.equals(response.orders.length, 2)
  })
  .and()
  .wait('user1', 'OrderChanged', response => {
    p.equals([get('orderId4'), get('orderId5')].indexOf(response.order.id) != -1, true);
  })
  .then()
  .send('user1', 'Orders', {status: map.OrderStatus.paid}, response => {
    p.equals(response.orders.length, 2)
    p.equals(response.orders[0].id, get('orderId4'))
    p.equals(response.orders[1].id, get('orderId5'))
  })
}
