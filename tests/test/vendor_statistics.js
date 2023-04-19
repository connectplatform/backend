module.exports = function (p) {
    var currentDate = +new Date();
    var dateOne = +new Date(+new Date() + 2 * 24 * 60 * 60 * 1000);

    var dayOfWeek = (date) => {
        return ['Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'][date.getDay()];
    };
    var dayFormatted = (d) => {
        const month = (d.getMonth() + 1 < 10 ? '0' + (d.getMonth() + 1) : (d.getMonth() + 1));
        const day = d.getDate() < 10 ? '0' + d.getDate() : d.getDate();
        return d.getFullYear() + '-' + month + '-' + day;
    };

    p
    .connect('user1')
    .connect('user2')
    .connect('user3')
    .send('user2', 'FeedPostCreate', {
        post: {
            type: 'tickets',
            title: 'Ticket 1',
            paymentInfo: {
                type: map.FeedPostPaymentType.ticketWithDate,
                price: 2000,
                currency: 'EUR',
                useQty: 10,
                availableDates: [
                    {date: dateOne, availableQty: 5}
                ]
            }
        }
    }, response => {
        p.set('productId1', response.post.id)
        p.equals(response.post.title, 'Ticket 1')
        p.equals(response.post.vendorId, '000000000000000000000002')
    })
    .then()
    .send('user2', 'FeedPostCreate', {
        post: {
            type: 'tickets',
            title: 'Ticket 2',
            paymentInfo: {
                type: map.FeedPostPaymentType.ticket,
                price: 2000,
                currency: 'EUR',
                useQty: 10,
            }
        }
    }, response => {
        p.set('productId2', response.post.id)
        p.equals(response.post.title, 'Ticket 2')
        p.equals(response.post.vendorId, '000000000000000000000002')
    })
    .then()
    .send('user1', 'CreateOrder', {productId: get('productId1'), qty: 1, date: dateOne}, response => {
        p.set('orderId1', response.order.id)
    })
    .then()
    .send('user1', 'CreateOrder', {productId: get('productId2'), qty: 1}, response => {
        p.set('orderId2', response.order.id)
    })
    .then()
    .send('user1', 'CreateOrder', {productId: get('productId1'), qty: 1, date: dateOne}, response => {
        p.equals(response.order.id, p.get('orderId1'))
    })
    .then()
    .send('user1', 'ChargeOrder', {orderIds: [get('orderId1'), get('orderId2')]}, response => {
        p.equals(response.orders.length, 2);
    })
    .then()
    .send('user3', 'CreateOrder', {productId: get('productId1'), qty: 1, date: dateOne}, response => {
        p.set('orderId3', response.order.id)
    })
    .then()
    .send('user3', 'ChargeOrder', {orderIds: [get('orderId3')]}, response => {
        p.equals(response.orders.length, 1);
    })
    .then()
    .set("afterChargeDate", () => +new Date())
    .send('user2', 'VendorStatistics', {
        from: currentDate,
        to: get("afterChargeDate"),
        groupBy: map.VendorStatisticsGroup.byProductByDay
    }, response => {
        const ticket1 = response.items.find((item) => 'Ticket 1' === item.name);
        const ticket2 = response.items.find((item) => 'Ticket 2' === item.name);

        p.equals(ticket1.total, 6000);
        p.equals(ticket1.currency, "EUR");
        p.equals(ticket1.qty, 3);
        p.equals(ticket1.items[0].id, dayFormatted(new Date()));
        p.equals(ticket1.items[0].name, dayOfWeek(new Date()));
        p.equals(ticket1.items[0].total, 6000);
        p.equals(ticket1.items[0].qty, 3);
        p.equals(ticket1.items[0].currency, "EUR");

        p.equals(ticket2.total, 2000);
        p.equals(ticket2.currency, "EUR");
        p.equals(ticket2.qty, 1);

        p.equals(ticket2.items[0].id, dayFormatted(new Date()));
        p.equals(ticket2.items[0].name, dayOfWeek(new Date()));
        p.equals(ticket2.items[0].total, 2000);
        p.equals(ticket2.items[0].qty, 1);
        p.equals(ticket2.items[0].currency, "EUR");
    })
    .then()
    .send('user2', 'VendorStatistics', {
        from: currentDate,
        to: get("afterChargeDate"),
        groupBy: map.VendorStatisticsGroup.byProduct
    }, response => {
        const ticket1 = response.items.find((item) => 'Ticket 1' === item.name);
        const ticket2 = response.items.find((item) => 'Ticket 2' === item.name);

        p.equals(ticket1.total, 6000);
        p.equals(ticket1.currency, "EUR");
        p.equals(ticket1.qty, 3);
        p.equals(ticket1.items.length, 2);
        const ticket1Item1 = ticket1.items.find((item) => p.get('orderId3') === item.id);
        p.equals(ticket1Item1.total, 2000);
        p.equals(ticket1Item1.currency, "EUR");
        p.equals(ticket1Item1.qty, 1);
        const ticket1Item2 = ticket1.items.find((item) => p.get('orderId1') === item.id);
        p.equals(ticket1Item2.total, 4000);
        p.equals(ticket1Item2.currency, "EUR");
        p.equals(ticket1Item2.qty, 2);

        p.equals(ticket2.total, 2000);
        p.equals(ticket2.currency, "EUR");
        p.equals(ticket2.qty, 1);
        p.equals(ticket2.items[0].total, 2000);
        p.equals(ticket2.items[0].currency, "EUR");
        p.equals(ticket2.items[0].qty, 1);
    })
    .then()
    .send('user2', 'VendorStatistics', {
        from: currentDate,
        to: get("afterChargeDate"),
        groupBy: map.VendorStatisticsGroup.byDay
    }, response => {
        p.equals(response.items.length, 1);
        p.equals(response.items[0].id, dayFormatted(new Date()));
        p.equals(response.items[0].name, dayOfWeek(new Date()));
        p.equals(response.items[0].total, 8000);
        p.equals(response.items[0].qty, 4);
        p.equals(response.items[0].currency, "EUR");
        p.equals(response.items[0].items.length, 3);

        const order1 = response.items[0].items.find((item) => p.get('orderId1') === item.id);
        const order2 = response.items[0].items.find((item) => p.get('orderId2') === item.id);
        const order3 = response.items[0].items.find((item) => p.get('orderId3') === item.id);

        p.equals(order1.id, p.get('orderId1'));
        p.equals(order2.id, p.get('orderId2'));
        p.equals(order3.id, p.get('orderId3'));

        p.equals(order1.total, 4000);
        p.equals(order1.qty, 2);
        p.equals(order2.total, 2000);
        p.equals(order2.qty, 1);
        p.equals(order3.total, 2000);
        p.equals(order3.qty, 1);
    })
}
