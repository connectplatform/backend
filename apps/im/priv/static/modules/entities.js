Entities = (function(){

    var modelsMap;

    getEmpty = function(Model){
      var result = {};

      for(var i = 0, len = Model.length; i<len; i++ ){
        if(Model[i].type == "set"){
          result[Model[i].name] = {type: Model[i].type, value: []};
        }else if(Model[i].type == "integer"){
          result[Model[i].name] = {type: Model[i].type, value: 0};
        }else if(Model[i].type == "sequence"){
          result[Model[i].name] = {type: Model[i].type, value: getEntity(Model[i].relatedType)};
        }else if(Model[i].type == "sequenceof"){
          result[Model[i].name] = {type: Model[i].type, value: [], relatedType: Model[i].relatedType};
        }else if(Model[i].type == "boolean"){
          result[Model[i].name] = {type: Model[i].type, value: false};
        }else{
          result[Model[i].name] = {type: "atom", value: "undefined"};
        }
      }

      return result;
    };

    getEntity = function(modelName, data){
      if(typeof data === 'undefined'){
        data = {};
      }else if(typeof data != 'object'){
        t.fail('Invalid data: ', data);
      }

      if(modelName in modelsMap){
        var Entity = getEmpty(modelsMap[modelName]);
        for(key in data){
          if(typeof Entity[key] == 'undefined'){
            t.fail('field: "' + key + '" not found in: ' + modelName);
          }else{
            Entity[key] = data[key];
          }
        };

        return Entity;
      }else{
        t.fail('entity ' + modelName + ' not found in map.json');
      }
    };

    setModelsMap = function(json){
      modelsMap = json;
    }

    return {
      get: getEntity,
      empty: getEmpty,
      setModelsMap: setModelsMap
    };
  })();