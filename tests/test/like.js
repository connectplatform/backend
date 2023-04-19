module.exports = function(p){
  p
  .connect('user1')
  // Create feed post with type tickets
  .send('user1', 'FeedPostCreate', {post: {type: 'tickets', title: 'Ticket 1', paymentInfo: {type: map.FeedPostPaymentType.ticket, price: 2000, currency: 'EUR', useQty: 10}}}, response => {
    p.set('productId1', response.post.id)
    p.equals(response.post.title, 'Ticket 1')
    p.equals(response.post.likes, 0)
    p.equals(response.post.isLiked, false)
  })
  // Check that user doesn't have liked posts with type tickets
  .then()
  .send('user1', 'FeedPost', {type: 'tickets', likedOnly: true}, response => {
    p.equals(response.posts.length, 0)
  })
  // Like product with type tickets
  .then()
  .send('user1', 'Like', {recordType: "feed_post", recordId: get('productId1')}, response => {
    p.equals(response.modelName, 'LikeResp')
  })
  .then()
  .send('user1', 'FeedPost', {postIds: [get('productId1')]}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].title, 'Ticket 1')
    p.equals(response.posts[0].likes, 1)
    p.equals(response.posts[0].isLiked, true)
  })
  // Try to like this post again
  .then()
  .send('user1', 'Like', {recordType: "feed_post", recordId: get('productId1')}, response => {
    p.equals(response.modelName, 'LikeResp')
  })
  // Ensure that the post did not liked twice
  .then()
  .send('user1', 'FeedPost', {postIds: [get('productId1')]}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.total, 1)
    p.equals(response.posts[0].title, 'Ticket 1')
    p.equals(response.posts[0].likes, 1)
    p.equals(response.posts[0].isLiked, true)
  })
  // Check that user has liked posts
  .then()
  .send('user1', 'FeedPost', {type: 'tickets', likedOnly: true}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].id, get('productId1'))
  })
  // Create Another feed post with type partners
  .then()
  .send('user1', 'FeedPostCreate', {post: {type: 'partners', title: 'Ticket 2', paymentInfo: {type: map.FeedPostPaymentType.product, price: 0, useQty: 0, discount: 0, currency: "EUR"}}}, response => {
    p.set('productId2', response.post.id)
    p.equals(response.post.title, 'Ticket 2')
    p.equals(response.post.likes, 0)
    p.equals(response.post.isLiked, false)
  })
  // Ensure that the product created successfully and does not have likes
  .then()
  .send('user1', 'FeedPost', {postIds: [get('productId2')]}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.total, 1)
    p.equals(response.posts[0].title, 'Ticket 2')
    p.equals(response.posts[0].likes, 0)
    p.equals(response.posts[0].isLiked, false)
  })
  // Check that user doesn't have liked posts with type partners
  .then()
  .send('user1', 'FeedPost', {type: 'partners', likedOnly: true}, response => {
    p.equals(response.posts.length, 0)
  })
  // Like product with type partners
  .then()
  .send('user1', 'Like', {recordType: "feed_post", recordId: get('productId2')}, response => {
    p.equals(response.modelName, 'LikeResp')
  })
  // Check that user has liked posts with type partners
  .then()
  .send('user1', 'FeedPost', {type: 'partners', likedOnly: true}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].id, get('productId2'))
    p.equals(response.posts[0].likes, 1)
    p.equals(response.posts[0].isLiked, true)
  })
  // Ensure that the user have two liked posts with types tickets and partners
  .then()
  .send('user1', 'FeedPost', {likedOnly: true}, response => {
    p.equals(response.posts.length, 2)
    p.equals(response.posts[0].likes, 1)
    p.equals(response.posts[0].isLiked, true)
    p.equals(response.posts[1].likes, 1)
    p.equals(response.posts[1].isLiked, true)
  })
  // dislike
  .then()
  .send('user1', 'Dislike', {recordType: "feed_post", recordId: get('productId1')}, response => {
    p.equals(response.modelName, 'DislikeResp')
  })
  // Ensure that the user have one liked posts when try to get all liked posts with all types
  .then()
  .send('user1', 'FeedPost', {likedOnly: true}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].likes, 1)
    p.equals(response.posts[0].isLiked, true)
  })
  .then()
  .send('user1', 'FeedPost', {postIds: [get('productId1')]}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].title, 'Ticket 1')
    p.equals(response.posts[0].likes, 0)
    p.equals(response.posts[0].isLiked, false)
  })
  //check that user doesn't have liked posts
  .then()
  .send('user1', 'FeedPost', {type: 'tickets', likedOnly: true}, response => {
    p.equals(response.posts.length, 0)
  })
  //like once again
  .then()
  .send('user1', 'Like', {recordType: "feed_post", recordId: get('productId1')}, response => {
    p.equals(response.modelName, 'LikeResp')
  })
  .then()
  .send('user1', 'FeedPost', {postIds: [get('productId1')]}, response => {
    p.equals(response.posts.length, 1)
    p.equals(response.posts[0].title, 'Ticket 1')
    p.equals(response.posts[0].likes, 1)
    p.equals(response.posts[0].isLiked, true)
  })
}
