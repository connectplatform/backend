var Hydrator = (function(){
  "use strict";
  var modelsMap;
  var module_id = "Hydrator";

  Number.prototype.isInteger = Number.prototype.isInteger || function (value) {
    return typeof value === "number" &&
      isFinite(value) &&
      Math.floor(value) === value;
  };

  return {
    setModelsMap: setModelsMap,
    encode: encode,
    decode: decode,
    getEntity: getEntity
  };

  function setModelsMap(maps) {
    modelsMap = maps;
  }

   function getEmpty(Model){
    var result = {};

    for(var i = 0, len = Model.length; i<len; i++ ){
      if(Model[i].type == "set"){
        result[Model[i].name] = [];
      }else if(Model[i].type == "integer"){
        result[Model[i].name] = 0;
      }else{
        result[Model[i].name] = {type: "atom", value: "undefined"};
      }
    }

    return result;
  }

  function getEntity(modelName, data){
    if(typeof data === 'undefined'){
      data = {};
    }else if(typeof data != 'object'){
      console.warn('Invalid data: ', data);
    }

    if(modelName in modelsMap){
      var Entity = getEmpty(modelsMap[modelName]);

      for(var key in data){
        if(typeof Entity[key] == 'undefined'){
          console.warn('field: "' + key + '" not found in: ' + modelName);
        }else{
          if(data[key]){
            Entity[key] = data[key];
          }
        }
      }
      return Entity;
    }else{
      console.warn('entity ' + modelName + ' not found in map.json');
    }
  }

  function encode(requestName, requestData){
    return prepare(requestName, requestData);
  }

  function decode(response){
    //return hydrate(response.value);
    return (response && typeof(response.value) == 'object' && response.length) ? hydrate(response.value) : '';
  }

  function hydrate(message) {
    var result = {},
      modelName = message[0].value,
      modelFields = modelsMap[modelName];

    for(var f = 0, flen = modelFields.length; f<flen; f++ ){

      var field = modelFields[f],          // model field
        data = message[f+1];    // field data


      if(data === undefined || data.value === "undefined"){
        result[field.name] = null;
        continue;
      }

      switch(field.type){
        case "set": // if field is set of relatedType models
        case "sequenceof":
          if (data.constructor === Array) {
            result[field.name] = [];
            for(var i = 0, len = data.length; i<len; i++ ){
              if(
                field.relatedType === 'integer' ||
                field.relatedType === 'real' ||
                field.relatedType === 'string' ||
                field.relatedType === 'boolean'
              ){
                result[field.name].push(data[i]);
              } else {
                result[field.name].push(hydrate(data[i].value));
              }
            }
          } else {
            throw new Error('Incorrect field ['+field.name+'] format: expect "Array", received: ' + typeof  data
              + ' | Module:(' + module_id.toUpperCase() + ', Line:48)');
          }
          break;
        case "sequence":
          result[field.name] = hydrate(data.value);
          break;
        case "integer":
          if(Number.isInteger(data)){
            result[field.name] = data;
          } else {
            throw new Error('Incorrect field ['+field.name+'] format: expect "integer", received: ' + typeof  data
              + ' | Module:(' + module_id.toUpperCase() + ', Line:86)');
          }
          break;
        case "string":
          if(typeof data === "string"){
            result[field.name] = data;
          } else if (typeof data === "object" && data instanceof Array) {
            if(data.length === 0){
              result[field.name] = undefined;
            } else {
              result[field.name] = Utf8ArrayToStr(data);
            }
          } else if (typeof data === "object" && data.value instanceof Array) {
            result[field.name] = Utf8ArrayToStr(data.value);
          } else {
            throw new Error('Incorrect field ['+field.name+'] format: expect "string", received: ' + typeof  data
              + ' | Module:(' + module_id.toUpperCase() + ', Line:76)');
          }
          break;
        case "boolean":
          if(typeof data === "boolean"){
            result[field.name] = data;
          } else {
            throw new Error('Incorrect field ['+field.name+'] format: expect "boolean", received: ' + typeof  data
              + ' | Module:(' + module_id.toUpperCase() + ', Line:86)');
          }
          break;
        case "generalizedtime":
          result[field.name] = moment.utc(data, 'YYYYMMDDHHmmss.SSS[Z]')
            .local()
            .format('YYYYMMDDHHmmss.SSS[Z]');
          break;
      }
    }
    return result;
  }

  function prepare(sequenceName, obj){
    var data =[];
    var sequenceMap = modelsMap[sequenceName];

    if(objectSize(obj) !== objectSize(sequenceMap)){
      console.warn("Error fields count not equal");
      console.warn(sequenceName);
      console.warn(obj);
      console.warn(sequenceMap);
      return;
    }

    data.push(Erl.atom(sequenceName));

    for(var f = 0, flen = sequenceMap.length; f<flen; f++ ){
      var fieldName = sequenceMap[f].name;
      if(obj[fieldName] === undefined){
        throw new Error("Field ["+fieldName+"] not exist " +
          ' | Module:(' + module_id.toUpperCase() + ', Line:132)');
      }
      data.push(convert(sequenceMap[f], obj[fieldName]));
    }

    function convert(fieldMap, value){
      switch (fieldMap.type) {
        case "set":
        case "sequenceof":
          if (value.constructor === Array) {
            var result = [];
            var itemMap = { type: fieldMap.relatedType };

            if(modelsMap.hasOwnProperty(fieldMap.relatedType)){
              for (var i = 0, len = value.length; i < len; i++) {
                result.push(prepare(fieldMap.relatedType, value[i]));
              }
            } else {
              for (var j = 0, lenj = value.length; j < lenj; j++) {
                result.push(convert(itemMap, value[j]));
              }
            }

          } else {
            throw new Error('Incorrect field [' + fieldMap.name + '] format: expect "Array", received: ' + typeof  value
              + ' | Module:(' + module_id.toUpperCase() + ', Line:142)');
          }

          return result;
        case "integer":
          return (typeof value === "string")? parseInt(value): value;
        case "real":
          return value;
        case "string":
          if(typeof value === "string"){
            return Erl.binary(utf8_toByteArray(value));
          } else if (typeof value === "object" && value.type === "atom") {
            return Erl.atom(value.value);
          } else {
            return value;
          }
        case "boolean":
          return value;
        case "generalizedtime":
          if (typeof value === "object" && value.type === "atom") {
            return Erl.atom(value.value);
          } else {
            return moment(value, 'YYYYMMDDHHmmss.SSS[Z]').utc().format('YYYYMMDDHHmmss.SSS[Z]');
          }
        case "sequence":
          return prepare(fieldMap.relatedType, value);
      }
    }
    //debugger;
    return Erl.tuple.apply(null, data);
  }

  function objectSize(obj) {
    var count = 0;

    if (typeof obj == "object") {
      if (Object.keys) {
        count = Object.keys(obj).length;
      } else if (window._) {
        count = _.keys(obj).length;
      } else if (window.$) {
        count = $.map(obj, function() { return 1; }).length;
      } else {
        for (var key in obj) if (obj.hasOwnProperty(key)) count++;
      }
    }

    return count;
  }

  function utf8_toByteArray(str) {
    var byteArray = [];
    if (str !== undefined && str !== null)
      for (var i = 0; i < str.length; i++)
        if (str.charCodeAt(i) <= 0x7F) byteArray.push(str.charCodeAt(i));
        else {
          var h = encodeURIComponent(str.charAt(i)).substr(1).split('%');
          for (var j = 0; j < h.length; j++) byteArray.push(parseInt(h[j], 16)); }
    return byteArray;
  }

  function Utf8ArrayToStr(array) {
    var out, i, len, c;
    var char2, char3;

    out = "";
    len = array.length;
    i = 0;
    while(i < len) {
      c = array[i++];
      switch(c >> 4)
      {
        case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
        // 0xxxxxxx
        out += String.fromCharCode(c);
        break;
        case 12: case 13:
        // 110x xxxx   10xx xxxx
        char2 = array[i++];
        out += String.fromCharCode(((c & 0x1F) << 6) | (char2 & 0x3F));
        break;
        case 14:
          // 1110 xxxx  10xx xxxx  10xx xxxx
          char2 = array[i++];
          char3 = array[i++];
          out += String.fromCharCode(((c & 0x0F) << 12) |
            ((char2 & 0x3F) << 6) |
            ((char3 & 0x3F) << 0));
          break;
      }
    }

    return out;
  }
})();