module.exports = function(p){
  p
  .connect('user1')
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: 'Ticket 2',
      paymentInfo: {
        type: map.FeedPostPaymentType.ticket,
        price: 2000,
        currency: 'EUR',
        useQty: 10,
    }
  }}, response => {
    p.set('productId', response.post.id)
  })
  //check that simple orders are merged
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1}, response => {
    p.set('orderId1', response.order.id)
    p.equals(response.modelName, 'CreateOrderResp')
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1}, response => {
    p.set('orderId2', response.order.id)
    p.equals(response.modelName, 'CreateOrderResp')
  })
  .then()
  .send('user1', 'Orders', {}, response => {
    p.equals(response.orders.length, 1)
    p.equals(response.orders[0].id, get('orderId1'))
  })
  //check that buyNow orders are not merged
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1, isBuyNow: true}, response => {
    p.set('orderId3', response.order.id)
    p.equals(response.modelName, 'CreateOrderResp')
  })
  .then()
  .send('user1', 'Orders', {}, response => {
    p.equals(response.orders.length, 2)
    p.equals(response.orders[0].id, get('orderId1'))
    p.equals(response.orders[1].id, get('orderId3'))
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 1, isBuyNow: true}, response => {
    p.set('orderId4', response.order.id)
    p.equals(response.modelName, 'CreateOrderResp')
  })
  .then()
  .send('user1', 'Orders', {}, response => {
    p.equals(response.orders.length, 3)
    p.equals(response.orders[0].id, get('orderId1'))
    p.equals(response.orders[1].id, get('orderId3'))
    p.equals(response.orders[2].id, get('orderId4'))
  })
}
