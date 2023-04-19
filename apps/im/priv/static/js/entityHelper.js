define(function(){

  var atomUndefined = {type: 'atom', value: 'undefined'}

  var getEmpty = function(Model){
    var result = {}

    for(var i = 0, len = Model.length; i < len; i++ ){
      if(Model[i].type == 'set'){
        result[Model[i].name] = []
      }else if(Model[i].type == 'boolean'){
        result[Model[i].name] = true
      }else if(Model[i].type == 'integer'){
        result[Model[i].name] = 0
      }else if(Model[i].type == 'sequence'){
        result[Model[i].name] = getEmpty(this.map[Model[i].relatedType])
      }else{
        result[Model[i].name] = atomUndefined
      }
    }

    return result
  }

  var getEntity = function(modelName, data){
    if(typeof data === 'undefined'){
      data = {}
    }else if(typeof data != 'object'){
      console.log('Invalid data: ', data)
    }

    if(modelName in this.map){
      var entity = getEmpty(this.map[modelName])

      return setDataInEntity(entity, data)
    }else{
      console.log('Entity: "' + modelName + '" not found in map.json')
    }
  }

  var setDataInEntity = function(entity, data){
    for(key in data){
      if(typeof(entity[key]) != 'undefined'){
        if ((data[key] instanceof Array) && (entity[key] instanceof Array)) {
          entity[key] = data[key]
        } else if (typeof(data[key]) == 'object' && entity[key] != atomUndefined){
          entity[key] = setDataInEntity(entity[key], data[key])
        } else {
          entity[key] = data[key]
        }
      }
    }

    return entity
  }

  var generateMessage = function generateMessage(feedType, feedId, payload){
      return {
        feedType: feedType,
        feedId: feedId,
        message: {
          id: '',
          type: 0,
          systemMessageType: '',
          systemMessageParams: [],
          kind: 1,
          created: 1500000000,
          origin: '',
          recipient: '',
          payload: payload ? payload : 'Hello, Mike',
          media: [{type: 1, link: "http://trinity.cdn.platon.sk/apcn4deq8lzj8plx4eno7tovioied7if6fr1444133218674718.jpeg"}],
          starred: false,
          geo: [45.2, 45.2],
        }
      };
    };

  var setMap = function(map){
    this.map = map
  }

  return {
    atomUndefined: atomUndefined,
    generateMessage: generateMessage,
    get: getEntity,
    setMap: setMap
  }
})
