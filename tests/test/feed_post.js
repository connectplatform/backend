module.exports = function(p){
  p
  .connect('user1')
  .send('user1', 'FeedPostCategoryCreate', {category: {tag: 'main', name: 'Foo'}}, response => {
    p.set('categoryId1', response.category.id)
    p.equals(response.category.name, 'Foo')
  })
  .then()
  .send('user1', 'FeedPostCategoryCreate', {category: {tag: 'main', name: 'Bar'}}, response => {
    p.set('categoryId2', response.category.id)
    p.equals(response.category.name, 'Bar')
  })
  //check feed post creation
  .then()
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: 'Ticket 1',
      categories: [get('categoryId1'), get('categoryId2')],
      tags: ['foo'],
      descr: 'Ticket descr 1',
      address: 'Ticket address 1',
      location: [49.43111190084455, 31.979190010472166],
      paymentInfo: {type: 1, currency: 'EUR', price: 1000}
    }
  }, response => {
    p.set('postId1', response.post.id)
    p.equals(response.post.type, 'tickets')
    p.equals(response.post.title, 'Ticket 1')
    p.deepEquals(response.post.tags, ['foo'])
    p.equals(response.post.descr, 'Ticket descr 1')
    p.equals(response.post.address, 'Ticket address 1')
    p.deepEquals(response.post.location, [49.43111190084455, 31.979190010472166])
    p.deepEquals(response.post.paymentInfo.type, 1)
    p.deepEquals(response.post.paymentInfo.currency, 'EUR')
    p.deepEquals(response.post.paymentInfo.price, 1000)
  })
  .then()
  .send('user1', 'FeedPostCreate', {
    post: {
      type: 'tickets',
      title: 'Ticket 2',
      categories: [get('categoryId1')],
      tags: ['foo', 'bar'],
      location: [50.401699, 30.252507],
      paymentInfo: {currency: 'EUR', price: 5000}
    }
  }, response => {
    p.set('postId2', response.post.id)
    p.equals(response.post.title, 'Ticket 2')
  })
  //check feed post tags retrieve
  .then()
  .send('user1', 'FeedPostTag', {type: 'tickets'}, response => {
    p.deepEquals(response.tags, ['foo', 'bar']);
  })
  //check get feed posts by type
  .then()
  .send('user1', 'FeedPost', {type: 'tickets'}, response => {
    p.equals(response.posts.length, 2);
  })
  //check get feed posts by post ids
  .then()
  .send('user1', 'FeedPost', {type: 'tickets', postIds: [get('postId2')]}, response => {
    p.equals(response.posts.length, 1);
    p.equals(response.posts[0].paymentInfo.currency, 'EUR');
    p.equals(response.posts[0].paymentInfo.price, 5000);
  })
  //check get feed posts by categories
  .then()
  .send('user1', 'FeedPost', {type: 'tickets', categories: [get('categoryId1')]}, response => {
    p.equals(response.posts.length, 2);
  })
  .then()
  .send('user1', 'FeedPost', {type: 'tickets', categories: [get('categoryId1'), get('categoryId2')]}, response => {
    p.equals(response.posts.length, 2);
  })
  .then()
  .send('user1', 'FeedPost', {type: 'tickets', categories: [get('categoryId2')]}, response => {
    p.equals(response.posts.length, 1);
  })
  .then()
  .send('user1', 'FeedPostCategoryDelete', {id: get('categoryId1')}, response => {
    p.equals(response.modelName, 'FeedPostCategoryDeleteResp')
  })
  .then()
  .send('user1', 'FeedPostCategoryDelete', {id: get('categoryId2')}, response => {
    p.equals(response.modelName, 'FeedPostCategoryDeleteResp')
  })
  //check get feed posts by tag
  .then()
  .send('user1', 'FeedPost', {type: 'tickets', tags: ['foo']}, response => {
    p.equals(response.posts.length, 2);
  })
  .then()
  .send('user1', 'FeedPost', {type: 'tickets', tags: ['bar']}, response => {
    p.equals(response.posts.length, 1);
  })
  //check get feed posts with geo sorting
  .then()
  .send('user1', 'FeedPost', {
    type: 'tickets',
    position: [49.43111190084455, 31.979190010472166],
    location: [49.43111190084455, 31.979190010472166]
  }, response => {
    p.equals(response.posts.length, 2);
    p.equals(response.posts[0].distance, 0);
    p.equals(response.posts[1].distance, 164147);
  })
  //
  .then()
  .send('user1', 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 3', tags: ['foo', 'bar'], location: [40, 40], paymentInfo: {currency: 'EUR', price: 5000}}}, response => {
    p.set('postId3', response.post.id)
    p.equals(response.post.title, 'Ticket 3')
  })
  .then()
  .send('user1', 'FeedPost', {
    type: 'tickets',
    position: [49.43111190084455, 31.979190010472166],
    location: [49.43111190084455, 31.979190010472166]
  }, response => {
    p.equals(response.posts.length, 3);
    p.equals(response.posts[0].id, get('postId1'));
    p.equals(response.posts[1].id, get('postId2'));
    p.equals(response.posts[2].id, get('postId3'));
  })
  //check updating feed post
  .then()
  .send('user1', 'FeedPostUpdate', {
    post: {
      id: get('postId1'),
      type: 'tickets X',
      title: 'Ticket 1 X',
      tags: ['foo', 'foo X'],
      descr: 'Ticket descr 1 X',
      workHours: 'Work hours X',
      userId: '000000000000000000000001',
      vendorId: '000000000000000000000002',
      address: 'Ticket address 1 X',
      location: [1.43111190084455, 1.979190010472166],
      paymentInfo: {type: 2, currency: 'USD', price: 2000}
    }
  }, response => {
    p.set('postId1', response.post.id)
    p.equals(response.post.type, 'tickets')
    p.equals(response.post.title, 'Ticket 1 X')
    p.deepEquals(response.post.tags, ['foo', 'foo X'])
    p.equals(response.post.descr, 'Ticket descr 1 X')
    p.equals(response.post.workHours, 'Work hours X')
    p.equals(response.post.userId, '000000000000000000000001')
    p.equals(response.post.vendorId, '000000000000000000000002')
    p.equals(response.post.address, 'Ticket address 1 X')
    p.deepEquals(response.post.location, [1.43111190084455, 1.979190010472166])
    p.deepEquals(response.post.paymentInfo.type, 2)
    p.deepEquals(response.post.paymentInfo.currency, 'USD')
    p.deepEquals(response.post.paymentInfo.price, 2000)
  })
  //check delete
  .then()
  .send('user1', 'FeedPost', {postIds: [get('postId3')]}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].id, get('postId3'))
  })
  .then()
  .send('user1', 'FeedPostDelete', {id: get('postId3')}, response => {
    p.equals(response.modelName, 'FeedPostDeleteResp')
  })
  .then()
  .send('user1', 'FeedPost', {postIds: [get('postId3')]}, response => {
    p.equals(response.posts.length, 0)
  })
  //check categories crud
  .then()
  .send('user1', 'FeedPostCategoryCreate', {category: {tag: 'foo', name: 'Category 1'}}, response => {
    p.set('categoryId1', response.category.id)
    p.equals(response.category.tag, 'foo')
    p.equals(response.category.name, 'Category 1')
  })
  .then()
  .send('user1', 'FeedPostCategoryCreate', {category: {tag: 'bar', name: 'Category 2'}}, response => {
    p.set('categoryId2', response.category.id)
    p.equals(response.category.tag, 'bar')
    p.equals(response.category.name, 'Category 2')
  })
  .then()
  .send('user1', 'FeedPostCategory', {tag: 'foo'}, response => {
    p.equals(response.categories.length, 1)
    p.equals(response.categories[0].id, get('categoryId1'))
  })
  .then()
  .send('user1', 'FeedPostCategory', {}, response => {
    p.equals(response.categories.length, 2)
  })
  .then()
  .send('user1', 'FeedPostCategoryUpdate', {category: {id: get('categoryId1'), tag: 'baz', name: 'Category 1'}}, response => {
    p.equals(response.category.tag, 'baz')
    p.equals(response.category.name, 'Category 1')
  })
  .then()
  .send('user1', 'FeedPostCategory', {tag: 'baz'}, response => {
    p.equals(response.categories.length, 1)
    p.equals(response.categories[0].id, get('categoryId1'))
    p.equals(response.categories[0].tag, 'baz')
    p.equals(response.categories[0].name, 'Category 1')
  })
  .then()
  .send('user1', 'FeedPostCategoryDelete', {id: get('categoryId1')}, response => {
    p.equals(response.modelName, 'FeedPostCategoryDeleteResp')
  })
  .then()
  .send('user1', 'FeedPostCategory', {tag: 'baz'}, response => {
    p.equals(response.categories.length, 0)
  })
}
