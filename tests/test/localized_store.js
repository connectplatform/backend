module.exports = function(p){
  p
  .connect('user1')
  .send('user1', 'LocalizedStoreCreate', {store: {name: 'Store 1', location: [40, 30]}}, response => {
    p.equals(response.store.name, 'Store 1')
    p.deepEquals(response.store.location, [40, 30])
  })
  .then()
  .send('user1', 'LocalizedStore', {location: [40, 30]}, response => {
    p.equals(response.stores.length, 1)
    p.equals(response.stores[0].name, 'Store 1')
    p.deepEquals(response.stores[0].location, [40, 30])
  })
  .then()
  .send('user1', 'LocalizedStore', {}, response => {
    p.equals(response.stores.length, 1)
  })
  .then()
  .send('user1', 'LocalizedStoreCreate', {store: {name: 'Store 2', location: [42, 32]}}, response => {
    p.equals(response.modelName, 'LocalizedStoreCreateResp')
    p.set('storeId2', response.store.id)
  })
  .then()
  .send('user1', 'LocalizedStore', {location: [40, 30]}, response => {
    p.equals(response.stores.length, 2)
    p.equals(response.stores[0].name, 'Store 1');
    p.equals(response.stores[0].distance, 0);
    p.equals(response.stores[1].name, 'Store 2');
    p.equals(response.stores[1].distance, 278677);
  })
  .then()
  .send('user1', 'LocalizedStoreUpdate', {store: {id: get('storeId2'), name: 'Store 22', location: [31, 32]}}, response => {
    p.equals(response.store.name, 'Store 22')
    p.deepEquals(response.store.location, [31, 32])
  })
  .then()
  .send('user1', 'LocalizedStore', {}, response => {
    p.equals(response.stores.length, 2)
    p.equals(response.stores[1].name, 'Store 22')
    p.deepEquals(response.stores[1].location, [31, 32])
  })
  .then()
  .send('user1', 'LocalizedStoreDelete', {id: get('storeId2')}, response => {
    p.equals(response.modelName, 'LocalizedStoreDeleteResp')
  })
  .then()
  .send('user1', 'LocalizedStore', {}, response => {
    p.equals(response.stores.length, 1)
    p.equals(response.stores[0].name, 'Store 1')
  })
}
