module.exports = function(p){
  const someDate = +new Date() + 60 * 60 * 1000;
  p
  .connect('user1')
  .connect('user2')
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: '10 Tickets',
      paymentInfo: {
        type: map.FeedPostPaymentType.ticketWithDate,
        price: 10,
        currency: 'EUR',
        availableDates: [
          {date: someDate, availableQty: 10}
        ]
      }
    }}, response => {
    p.set('productId', response.post.id)
  })
  .then()
  .send('user2', 'CreateOrder', {ref: 1,  productId: get('productId'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {ref: 2,  productId: get('productId'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {ref: 3,  productId: get('productId'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {ref: 4,  productId: get('productId'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {ref: 5,  productId: get('productId'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {ref: 6,  productId: get('productId'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {ref: 7,  productId: get('productId'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {ref: 8,  productId: get('productId'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {ref: 9,  productId: get('productId'), qty: 1, date: someDate}, r => r)
  .send('user2', 'CreateOrder', {ref: 10, productId: get('productId'), qty: 1, date: someDate}, r => r)
  .then(responses => {
    p.equals(responses.length, 10);
    responses = responses.sort((a, b) => {
      if (a.order.qty > b.order.qty) {
        return 1;
      } if (a.order.qty < b.order.qty) {
        return -1;
      }
      return 0;
    });
    p.equals(responses.every(i => i.modelName === 'CreateOrderResp'), true);
    p.equals(JSON.stringify(responses.map(i => i.order.qty)), JSON.stringify([1,2,3,4,5,6,7,8,9,10]));
    p.equals(JSON.stringify(responses.map(i => i.order.total)), JSON.stringify([10,20,30,40,50,60,70,80,90,100]));
    p.equals(responses.every(i => i.order.price === 10), true);
    p.equals(responses.every(i => i.order.status === 1), true);
    p.equals(responses.every(i => i.order.purchaseDate === null), true);
    p.equals(responses.every(i => i.order.vendorId === '000000000000000000000001'), true);
    p.equals(responses.every(i => i.order.productId === p.get('productId')), true);
  })
  .then()
  .send('user2', 'Orders', {}, response => {
      p.equals(response.orders.length, 1);
  })
}
