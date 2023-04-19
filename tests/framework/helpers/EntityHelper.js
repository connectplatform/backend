module.exports = (function(){

  var atomUndefined = {atom: 'undefined'};

  var getEmpty = function(ModelName, Model){
    var result = {};
    for(var i = 0, len = Model.length; i < len; i++ ){
      if(Model[i].type === 'set' || Model[i].type === 'setof' || Model[i].type === 'sequenceof'){
        result[Model[i].name] = []
      }else if(Model[i].type === 'boolean'){
        result[Model[i].name] = false
      }else if(Model[i].type === 'integer'){
        result[Model[i].name] = 0
      }else if(Model[i].type === 'sequence'){
      	if (Model[i].relatedType === ModelName) {
					result[Model[i].name] = atomUndefined;
				} else {
					result[Model[i].name] = getEmpty(Model[i].relatedType, map[Model[i].relatedType])
				}
      }else{
        result[Model[i].name] = atomUndefined
      }
    }

    return result
  };

  var getEntity = function(modelName, data){
    if(typeof data === 'undefined'){
      data = {}
    }else if(typeof data != 'object'){
      console.log('Invalid data: ', data)
    }

    if(modelName in map){
      var entity = getEmpty(modelName, map[modelName]);
      return setDataInEntity(entity, data)
    }else{
      console.log('Entity: "' + modelName + '" not found in map.json')
    }
  };

  var setDataInEntity = function(entity, data){
    // console.log('ENTITY: ', entity, 'DATA: ', data);
    for(key in data){
      // console.log('KEY: ', key, 'DATA: ', data[key], 'ENTITY PREV: ', entity[key]);
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
  };

  var generateMessage = function generateMessage(feedType, feedId, payload, replyToId){
      var media = generateMedia(1, "http://trinity.cdn.platon.sk/apcn4deq8lzj8plx4eno7tovioied7if6fr1444133218674718.jpeg");

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
          media: [media],
          starred: false,
          geo: [45.2, 45.2],
          deleted: 0,
          deletedBy: [],
					forward: atomUndefined,
					reply: replyToId ? {id: replyToId} : atomUndefined,
        }
      };
    };

  var generateMedia = function generateMedia(type, link) {
    return {
      type: type,
      link: link,
      thumbnail: link,
      width: 0,
      height: 0,
      duration: 0,
      name: '',
      size: 0,
	    ogData: atomUndefined,
	    style: '',
	    callId: '',
	    taskId: '',
	    contactId: '',
	    messageId: ''
    };
  };

  return {
    atomUndefined: atomUndefined,
    generateMessage: generateMessage,
    get: getEntity
  }
})();
