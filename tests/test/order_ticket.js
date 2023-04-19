module.exports = function(p){
  p
  .connect('user1')
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: 'Ticket 1',
      paymentInfo: {
        type: map.FeedPostPaymentType.ticket,
        price: 2000,
        currency: 'EUR',
        useQty: 10,
    }
  }}, response => {
    p.set('productId', response.post.id)
  })
  .then()
  .send('user1', 'CreateOrder', {productId: get('productId'), qty: 3}, response => {
    p.equals(response.modelName, 'CreateOrderResp')
  })
}
