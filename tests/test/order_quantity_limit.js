module.exports = function(p){
  const someDate = +new Date() + 60 * 60 * 1000;
  p
  .connect('user1')
  .connect('user2')
  .connect('user3')
  .connect('user4')
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: 'One Ticket',
      paymentInfo: {
        type: map.FeedPostPaymentType.ticketWithDate,
        price: 10,
        currency: 'EUR',
        availableDates: [
          {date: someDate, availableQty: 1}
        ]
      }
    }}, response => {
		p.equals(response.modelName, 'FeedPostCreateResp');
    p.set('productId', response.post.id)
  })
  .then()
  .send('user2', 'CreateOrder', {productId: get('productId'), qty: 2, date: someDate}, r => {
    p.equals(r.modelName, 'ErrorResp');
    p.equals(r.code, map.ErrorCode.qtyNotAvailable)
  })
  .then()
  .send('user2', 'CreateOrder', {productId: get('productId'), qty: 1, date: someDate}, r => {
      p.set('user2_OrderId1', r.order.id);
      return r;
  })
  .send('user3', 'CreateOrder', {productId: get('productId'), qty: 1, date: someDate}, r => {
      p.set('user3_OrderId1', r.order.id);
      return r;
  })
  .send('user4', 'CreateOrder', {productId: get('productId'), qty: 1, date: someDate}, r => {
      p.set('user4_OrderId1', r.order.id);
      return r;
  })
  .then(responses => {
      p.equals(responses.every(i => i.modelName === 'CreateOrderResp'), true);
  })
  .send('user2', 'ChargeOrder', {orderIds: [get('user2_OrderId1')]}, r => r)
  .send('user3', 'ChargeOrder', {orderIds: [get('user3_OrderId1')]}, r => r)
  .send('user4', 'ChargeOrder', {orderIds: [get('user4_OrderId1')]}, r => r)
  .then((responses) => {
    const successResponses = responses.filter(response => response.modelName === 'ChargeOrderResp');
    const errorResponses = responses.filter(response => response.modelName === 'ErrorResp');
    p.equals(successResponses.length, 1);
    p.equals(errorResponses.length, responses.length - 1);
  })
  .then()
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: 'One Ticket',
      paymentInfo: {
        type: map.FeedPostPaymentType.ticketWithDate,
        price: 10,
        currency: 'EUR',
        availableDates: [
          {date: someDate, availableQty: 1}
        ]
      }
    }}, response => {
    p.set('productId2', response.post.id)
  })
  .then()
  .send('user2', 'CreateOrder', {productId: get('productId2'), qty: 2, date: someDate}, r => {
    p.equals(r.modelName, 'ErrorResp');
    p.equals(r.code, map.ErrorCode.qtyNotAvailable)
  })
  .then()
  .send('user2', 'CreateOrder', {productId: get('productId2'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {productId: get('productId2'), qty: 1, date: someDate}, r => r)
  .then(responses => {
    const successResponses = responses.filter(response => response.modelName === 'CreateOrderResp');
    const errorResponses = responses.filter(response => response.modelName === 'ErrorResp');
    p.equals(successResponses.length, 1);
    p.equals(errorResponses.length, 1);
    p.equals(errorResponses[0].code, map.ErrorCode.qtyNotAvailable)
  })
}
