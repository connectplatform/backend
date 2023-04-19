module.exports = function(p){
	const vendorId = '000000000000000000000001';

  const someDate = +new Date() + 1 * 60 * 60 * 1000;
  const someAnotherDate = +new Date() + 2 * 60 * 60 * 1000;
	const thisDateAreDeletedInFuture = +new Date() + 3 * 60 * 60 * 1000;

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
          {date: someDate, 			  availableQty: 5},
          {date: someAnotherDate, availableQty: 3},
          {date: thisDateAreDeletedInFuture, availableQty: 3},
        ]
      }
    }}, response => {
  	p.equals(response.modelName, 'FeedPostCreateResp');

		const post = response.post;
		p.equals(post.paymentInfo.availableDates.length, 3);
		p.equals(post.paymentInfo.price, 10);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).availableQty, 5);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).availableToBuyQty, 5);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).reservedQty, 0);

		p.equals(post.paymentInfo.availableDates.find(i => i.date === someAnotherDate).availableQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someAnotherDate).availableToBuyQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someAnotherDate).reservedQty, 0);

		p.equals(post.paymentInfo.availableDates.find(i => i.date === thisDateAreDeletedInFuture).availableQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === thisDateAreDeletedInFuture).availableToBuyQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === thisDateAreDeletedInFuture).reservedQty, 0);

    p.set('productId', response.post.id)
  })
  .then() // user2 createOrder for someDate with qty 2
  .send('user2', 'CreateOrder', {productId: get('productId'), qty: 2, date: someDate}, response => {
  	p.equals(response.modelName, 'CreateOrderResp');
  	p.equals(response.order.productId, p.get('productId'));
		p.equals(response.order.status, map.OrderStatus.pending);
  	p.equals(response.order.qty, 2);
  	p.equals(response.order.availableUseQty, 0);
  	p.equals(response.order.price, 10);
  	p.equals(response.order.total, 20);
  	p.equals(response.order.vendorId, vendorId);

		p.set('user2_OrderId1', response.order.id);
  })
	.then() // user2 createOrder for someAnotherDate with qty 2
	.send('user2', 'CreateOrder', {productId: get('productId'), qty: 2, date: someAnotherDate}, response => {
		p.equals(response.modelName, 'CreateOrderResp');
		p.equals(response.order.productId, p.get('productId'));
		p.equals(response.order.status, map.OrderStatus.pending);
		p.equals(response.order.qty, 2);
		p.equals(response.order.availableUseQty, 0);
		p.equals(response.order.price, 10);
		p.equals(response.order.total, 20);
		p.equals(response.order.vendorId, vendorId);

		p.set('user2_OrderId2', response.order.id);
	})
	.then() // user3 createOrder for someDate with qty 3
	.send('user3', 'CreateOrder', {productId: get('productId'), qty: 3, date: someDate}, response => {
		p.equals(response.modelName, 'CreateOrderResp');
		p.equals(response.order.productId, p.get('productId'));
		p.equals(response.order.status, map.OrderStatus.pending);
		p.equals(response.order.qty, 3);
		p.equals(response.order.availableUseQty, 0);
		p.equals(response.order.price, 10);
		p.equals(response.order.total, 30);
		p.equals(response.order.vendorId, vendorId);

		p.set('user3_OrderId1', response.order.id);
	})
	.then() // user4 createOrder for thisDateAreDeletedInFuture with qty 3
	.send('user3', 'CreateOrder', {productId: get('productId'), qty: 3, date: thisDateAreDeletedInFuture}, response => {
		p.equals(response.modelName, 'CreateOrderResp');
		p.equals(response.order.productId, p.get('productId'));
		p.equals(response.order.status, map.OrderStatus.pending);
		p.equals(response.order.qty, 3);
		p.equals(response.order.availableUseQty, 0);
		p.equals(response.order.price, 10);
		p.equals(response.order.total, 30);
		p.equals(response.order.vendorId, vendorId);

		p.set('user4_OrderId1', response.order.id);
	})
	.then() // check feed post availableQty not changed
	.send('user1', 'FeedPost', {postIds: [get('productId')]}, response => {
		p.equals(response.modelName, 'FeedPostResp');
		p.equals(response.posts.length, 1);
		const post = response.posts[0];
		p.equals(post.paymentInfo.availableDates.length, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).availableQty, 5);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).availableToBuyQty, 5);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).reservedQty, 0);

		p.equals(post.paymentInfo.availableDates.find(i => i.date === someAnotherDate).availableQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someAnotherDate).availableToBuyQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someAnotherDate).reservedQty, 0);

		p.equals(post.paymentInfo.availableDates.find(i => i.date === thisDateAreDeletedInFuture).availableQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === thisDateAreDeletedInFuture).availableToBuyQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === thisDateAreDeletedInFuture).reservedQty, 0);
	})
	.then() // user2 paid one of two orders
	.send('user2', 'ChargeOrder', {orderIds: [get('user2_OrderId1')]}, response => {
		p.equals(response.modelName, 'ChargeOrderResp');
		const orderPaid = response.orders.find(o => o.id === p.get('user2_OrderId1'));
		p.equals(orderPaid.qty, 2);
		p.equals(orderPaid.availableUseQty, 2);
		p.equals(orderPaid.status, map.OrderStatus.paid);
		p.equals(orderPaid.date, someDate);
		p.equals(orderPaid.price, 10);
		p.equals(orderPaid.total, 20);
	})
	.then() // check feed post availableQty changed for someDate
	.send('user1', 'FeedPost', {postIds: [get('productId')]}, response => {
    p.equals(response.modelName, 'FeedPostResp');
		p.equals(response.posts.length, 1);

    const post = response.posts[0];
		p.equals(post.paymentInfo.availableDates.length, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).availableQty, 5);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).availableToBuyQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).reservedQty, 2);

		p.equals(post.paymentInfo.availableDates.find(i => i.date === someAnotherDate).availableQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someAnotherDate).availableToBuyQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === someAnotherDate).reservedQty, 0);

		p.equals(post.paymentInfo.availableDates.find(i => i.date === thisDateAreDeletedInFuture).availableQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === thisDateAreDeletedInFuture).availableToBuyQty, 3);
		p.equals(post.paymentInfo.availableDates.find(i => i.date === thisDateAreDeletedInFuture).reservedQty, 0);
	})
  // .then()
  // .send('user1', 'FeedPostUpdate', {post: {
	// 		id: get('productId'),
	// 		type: 'tickets',
	// 		title: 'One Ticket',
	// 		paymentInfo: {
	// 			type: map.FeedPostPaymentType.ticketWithDate,
	// 			price: 20,
	// 			currency: 'EUR',
	// 			availableDates: [
	// 				{date: someDate, 			  availableQty: 4},
	// 				{date: someAnotherDate, availableQty: 2},
	// 				// thisDateAreDeletedInFuture are deleted
	// 			]
	// 		}
	// 	}}, response => {
	// 	p.equals(response.modelName, 'FeedPostUpdateResp');
	//
	// 	const post = response.post;
	// 	p.equals(post.paymentInfo.price, 20);
	// 	p.equals(post.paymentInfo.availableDates.length, 1);
	// 	p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).availableQty, 3);
	// 	p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).availableToBuyQty, 1); // TODO FIX availableToBuyQty
	// 	p.equals(post.paymentInfo.availableDates.find(i => i.date === someDate).reservedQty, 2);
	// })
	// .then() // check that order with status paid for user 2 not changed and order with status pending changed
	// .send('user2', 'Orders', {}, response => {
	// 	p.equals(response.modelName, 'OrdersResp');
	// 	p.equals(response.orders.length, 2);
	// 	const orderPaid = response.orders.find(o => o.id === p.get('user2_OrderId1'));
	// 	const orderPending = response.orders.find(o => o.id === p.get('user2_OrderId2'));
	//
	// 	p.equals(orderPaid.productId, p.get('productId'));
	// 	p.equals(orderPaid.vendorId, vendorId);
	// 	p.equals(orderPaid.qty, 2);
	// 	p.equals(orderPaid.availableUseQty, 2);
	// 	p.equals(orderPaid.price, 10);
	// 	p.equals(orderPaid.total, 20);
	// 	p.equals(orderPaid.status, map.OrderStatus.paid);
	// 	p.equals(orderPaid.date, someDate);
	//
	// 	p.equals(orderPending.productId, p.get('productId'));
	// 	p.equals(orderPending.vendorId, vendorId);
	// 	p.equals(orderPending.qty, 2);
	// 	p.equals(orderPending.availableUseQty, 0);
	// 	p.equals(orderPending.price, 20); // TODO Must changed
	// 	p.equals(orderPending.total, 40); // TODO Must changed
	// 	p.equals(orderPending.status, map.OrderStatus.pending);
	// 	p.equals(orderPending.date, someAnotherDate);
	// })
	// .then() // check that order with status pending changed
	// .send('user3', 'Orders', {}, response => {
	// 	p.equals(response.modelName, 'OrdersResp');
	// 	p.equals(response.orders.length, 1);
	// 	p.equals(response.orders[0].productId, p.get('productId'));
	// 	p.equals(response.orders[0].vendorId, vendorId);
	// 	p.equals(response.orders[0].qty, 3);
	// 	p.equals(response.orders[0].price, 20); // TODO Must changed
	// 	p.equals(response.orders[0].total, 60); // TODO Must changed
	// 	p.equals(response.orders[0].status, map.OrderStatus.pending);
	// 	p.equals(response.orders[0].date, someDate);
	// 	p.equals(response.orders[0].availableUseQty, 0);
	// })
	// .then()
	// .send('user3', 'ChargeOrder', {orderIds: [get('user3_OrderId1')]}, response => {
	// 	p.equals(response.modelName, 'ErrorResp');
	// })
	// .then()
	// .send('user2', 'ChargeOrder', {orderIds: [get('user2_OrderId2')]}, response => {
	// 	p.equals(response.modelName, 'ChargeOrderResp');
	// 	const orderPaid = response.orders.find(o => o.id === p.get('user2_OrderId2'));
	// 	p.equals(orderPaid.qty, 2);
	// 	p.equals(orderPaid.availableUseQty, 2);
	// 	p.equals(orderPaid.status, map.OrderStatus.paid);
	// 	p.equals(orderPaid.date, someDate);
	// 	p.equals(orderPaid.price, 20);
	// 	p.equals(orderPaid.total, 40);
	// })
	// .then()
	// .send('user2', 'Orders', {}, response => {
	// 	p.equals(response.modelName, 'OrdersResp');
	// })
}
