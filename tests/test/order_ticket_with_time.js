module.exports = function(p){
  var dateOne = +new Date(+new Date() + 1 * 24*60*60*1000);
  var dateTwo = +new Date(+new Date() + 2 * 24*60*60*1000);
  var oldDate = +new Date(+new Date() - 1000000);

  p
  //check that I can't create order whan no available qty
  .connect('user1')
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: 'Ticket 2',
      paymentInfo: {
        type: map.FeedPostPaymentType.ticketWithDate,
        price: 2000,
        currency: 'EUR',
        useQty: 10,
        availableDates: [
          {date: dateOne, availableQty: 5},
          {date: dateTwo, availableQty: 7},
          {date: oldDate, availableQty: 100},
        ]
    }
  }}, response => {
    p.set('productId', response.post.id)
    p.equals(response.post.paymentInfo.availableDates.length, 3)
    p.equals(response.post.paymentInfo.availableDates[0].date, dateOne)
    p.equals(response.post.paymentInfo.availableDates[0].availableQty, 5)
    p.equals(response.post.paymentInfo.availableDates[0].availableToBuyQty, 5)
    p.equals(response.post.paymentInfo.availableDates[0].reservedQty, 0)
    p.equals(response.post.paymentInfo.availableDates[1].date, dateTwo)
    p.equals(response.post.paymentInfo.availableDates[1].availableQty, 7)
    p.equals(response.post.paymentInfo.availableDates[1].availableToBuyQty, 7)
    p.equals(response.post.paymentInfo.availableDates[1].reservedQty, 0)
    p.equals(response.post.paymentInfo.availableDates[2].date, oldDate)
    p.equals(response.post.paymentInfo.availableDates[2].availableQty, 100)
    p.equals(response.post.paymentInfo.availableDates[2].availableToBuyQty, 100)
    p.equals(response.post.paymentInfo.availableDates[2].reservedQty, 0)
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 6, date: dateOne}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.qtyNotAvailable)
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 3, date: dateOne}, response => {
    p.set('orderId', response.order.id)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 3)
    p.equals(response.order.date, dateOne)
  })
  .then()
  .send('user1', 'FeedPost', {postIds: [get('productId')]}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].paymentInfo.availableDates.length, 3)
    p.equals(response.posts[0].paymentInfo.availableDates[0].availableQty, 5)
    p.equals(response.posts[0].paymentInfo.availableDates[0].availableToBuyQty, 5)
    p.equals(response.posts[0].paymentInfo.availableDates[0].reservedQty, 0)
  })
  .then()
  .send('user1', 'DeleteOrder', {orderId: get('orderId')}, response => {
    p.equals(response.modelName, 'DeleteOrderResp')
  })
  .then()
  .send('user1', 'FeedPost', {postIds: [get('productId')]}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].paymentInfo.availableDates.length, 3)
    p.equals(response.posts[0].paymentInfo.availableDates[0].availableQty, 5)
    p.equals(response.posts[0].paymentInfo.availableDates[0].availableToBuyQty, 5)
    p.equals(response.posts[0].paymentInfo.availableDates[0].reservedQty, 0)
  })
  //check that date is required
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 2}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidDate)
  })
  //check that pending orders with different dates doesn't merge
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1, date: dateOne}, response => {
    p.set('orderId2', response.order.id)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 1)
    p.equals(response.order.date, dateOne)
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1, date: dateTwo}, response => {
    p.set('orderIdTemp', response.order.id)
    p.equals(response.order.id != get('orderId2'), true)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 1)
    p.equals(response.order.date, dateTwo)
  })
  .then()
  .send('user1', 'DeleteOrder', {orderId: get('orderIdTemp')}, response => {
    p.equals(response.modelName, 'DeleteOrderResp')
  })
  //check that pending orders with equal dates merge
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1, date: dateOne}, response => {
    p.equals(response.order.id, get('orderId2'))
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 2)
    p.equals(response.order.date, dateOne)
  })
  //check that paid orders with different dates doesn't merge
  .then()
  .send('user1', 'ChargeOrder', {orderIds: [get('orderId2')]}, response => {
    p.equals(response.orders.length, 1)
  })
  .then()
  .send('user1', 'FeedPost', {postIds: [get('productId')]}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].paymentInfo.availableDates.length, 3)
    p.equals(response.posts[0].paymentInfo.availableDates[0].availableQty, 5)
    p.equals(response.posts[0].paymentInfo.availableDates[0].availableToBuyQty, 3)
    p.equals(response.posts[0].paymentInfo.availableDates[0].reservedQty, 2)
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1, date: dateTwo}, response => {
    p.set('orderId3', response.order.id)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 1)
    p.equals(response.order.date, dateTwo)
  })
  .then()
  .send('user1', 'ChargeOrder', {orderIds: [get('orderId3')]}, response => {
    p.equals(response.orders.length, 1)
    p.equals(response.orders[0].id, get('orderId3'))
  })
  //check that paid orders with equals dates merge
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1, date: dateTwo}, response => {
    p.set('orderId4', response.order.id)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 1)
    p.equals(response.order.date, dateTwo)
  })
  .then()
  .send('user1', 'ChargeOrder', {orderIds: [get('orderId4')]}, response => {
    p.equals(response.orders.length, 1)
    p.equals(response.orders[0].id, get('orderId3'))
    p.equals(response.orders[0].qty, 2)
    p.equals(response.orders[0].date, dateTwo)
  })
  .and()
  .wait('user1', 'OrderDeleted', response => {
    p.equals(response.orderId, get('orderId4'))
  })
  .and()
  //TODO: why we also get here OrderChanged for orderId4
  .wait('user1', {name: 'OrderChanged', filter: data => data.order.id === get('orderId3')}, response => {
    p.equals(response.order.id, get('orderId3'))
    p.equals(response.order.qty, 2)
    p.equals(response.order.total, 4000)
    p.equals(response.order.date, dateTwo)
  })
  //check that change order works
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1, date: dateTwo}, response => {
    p.set('orderId5', response.order.id)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 1)
    p.equals(response.order.date, dateTwo)
  })
  .and()
  .wait('user1', 'OrderChanged', response => {
    p.equals(response.order.qty, 1)
    p.equals(response.order.total, 2000)
    p.equals(response.order.date, dateTwo)
  })
  .then()
  .send('user1', 'ChangeOrder', {order: {id: get('orderId5'), qty: 2, date: dateTwo}}, response => {
    p.equals(response.order.qty, 2)
    p.equals(response.order.total, 4000)
    p.equals(response.order.date, dateTwo)
  })
  .and()
  .wait('user1', 'OrderChanged', response => {
    p.equals(response.order.id, get('orderId5'))
    p.equals(response.order.qty, 2)
    p.equals(response.order.total, 4000)
    p.equals(response.order.date, dateTwo)
  })
  //check that change order respects availableToBuyQty
  .then()
  .send('user1', 'ChangeOrder', {order: {id: get('orderId5'), qty: 99, date: dateTwo}}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.qtyNotAvailable)
  })
  // try create order with not valid date
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 6, date: oldDate}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidDate)
  })
  .then()
  .set('expiredInFewSecondsDate', () => +new Date() + 5000)
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: 'Ticket 3',
      paymentInfo: {
        type: map.FeedPostPaymentType.ticketWithDate,
        price: 2000,
        currency: 'EUR',
        useQty: 10,
        availableDates: [
          {date: p.get('expiredInFewSecondsDate'), availableQty: 5}
        ]
      }
    }}, response => {
    // console.log('DIFF TIME FeedPostCreate', p.get('expiredInFewSecondsDate') -  +new Date())
    p.set('productId2', response.post.id)
    p.equals(response.post.paymentInfo.availableDates.length, 1)
    p.equals(response.post.paymentInfo.availableDates[0].date, p.get('expiredInFewSecondsDate'))
    p.equals(response.post.paymentInfo.availableDates[0].availableQty, 5)
    p.equals(response.post.paymentInfo.availableDates[0].availableToBuyQty, 5)
    p.equals(response.post.paymentInfo.availableDates[0].reservedQty, 0)

  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId2'), qty: 1, date: get('expiredInFewSecondsDate')}, response => {
      // console.log('DIFF TIME CreateOrder', p.get('expiredInFewSecondsDate') -  +new Date())
      p.set('expiredOrderId', response.order.id)
      p.equals(response.modelName, 'CreateOrderResp')
      p.equals(response.order.productId, get('productId2'))
      p.equals(response.order.qty, 1)
      p.equals(response.order.date, p.get('expiredInFewSecondsDate'))
  })
  .then()
  .timeout(5000)
  .send('user1', 'ChargeOrder', {orderIds: [get('expiredOrderId')]}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidDate)
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1, date: dateOne}, response => {
    p.set('anotherValidOrderId', response.order.id)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 1)
    p.equals(response.order.date, dateOne)
  })
  .then()
  .send('user1', 'ChargeOrder', {orderIds: [get('expiredOrderId'), get('anotherValidOrderId')]}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidDate)
  })
}
