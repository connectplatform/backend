module.exports = function(p){
  var currentTime = +new Date();
  var expiredAfterFewSecondsTime = +new Date(currentTime + 8000);
  var notExpiredTime = +new Date(currentTime + 24*60*60*1000);

  p
  .connect('user1')
  // Create feed post with two dates where expiredAfterFewSecondsTime will be expired later
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: 'Tickets with date',
      paymentInfo: {
        type: map.FeedPostPaymentType.ticketWithDate,
        price: 2000,
        currency: 'EUR',
        useQty: 10,
        availableDates: [
          {date: notExpiredTime, availableQty: 5},
          {date: expiredAfterFewSecondsTime, availableQty: 5}
        ]
    }
  }}, response => {
    p.set('productId', response.post.id)
    p.equals(response.post.paymentInfo.availableDates.length, 2)

    p.equals(response.post.paymentInfo.availableDates[0].date, notExpiredTime)
    p.equals(response.post.paymentInfo.availableDates[0].availableQty, 5)
    p.equals(response.post.paymentInfo.availableDates[0].availableToBuyQty, 5)
    p.equals(response.post.paymentInfo.availableDates[0].reservedQty, 0)

    p.equals(response.post.paymentInfo.availableDates[1].date, expiredAfterFewSecondsTime)
    p.equals(response.post.paymentInfo.availableDates[1].availableQty, 5)
    p.equals(response.post.paymentInfo.availableDates[1].availableToBuyQty, 5)
    p.equals(response.post.paymentInfo.availableDates[1].reservedQty, 0)
  })
  .then()
  // successfully create order in expiredAfterFewSecondsTime date
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 3, date: expiredAfterFewSecondsTime}, response => {
    p.set('expiredOrderId', response.order.id)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 3)
    p.equals(response.order.date, expiredAfterFewSecondsTime)
  })
  .then()
  // ensure that order are not expired
  .send('user1', 'Orders', {orderIds: [get('expiredOrderId')]}, response => {
    p.equals(response.orders[0].status, 1)
  })
  .then()
  .timeout(8000)
  // wait when order was expired and check it
  .send('user1', 'Orders', {orderIds: [get('expiredOrderId')]}, response => {
    p.equals(response.orders[0].status, 3)
  })
  .then()
  // try to create order in expired time
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 3, date: expiredAfterFewSecondsTime}, response => {
    p.equals(response.modelName, 'ErrorResp')
    p.equals(response.code, map.ErrorCode.invalidDate)
  })
  .then()
  // Create another order with not expired time
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 3, date: notExpiredTime}, response => {
    p.set('notExpiredOrderId', response.order.id)
    p.equals(response.order.productId, get('productId'))
    p.equals(response.order.qty, 3)
    p.equals(response.order.date, notExpiredTime)
  })
  .then()
  // ensure that order are not expired
  .send('user1', 'Orders', {orderIds: [get('notExpiredOrderId')]}, response => {
    p.equals(response.orders[0].status, 1)
  })
}

