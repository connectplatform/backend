/* jshint bitwise: false */
// const moment = require('moment');
// const map = require('../../content/data/map.json');
// const Erl = require('../../utils/erlb');

var Hydrator = (function(){
  "use strict";

  var map;

  return {
    encode,
    decode,
    generateEntity,
    fetchMap,
  };

  function fetchMap(cb) {
    const xhr = new XMLHttpRequest();
    xhr.onreadystatechange = () => {
      if (xhr.readyState === 4) {
        if (xhr.status === 200) {
          map = JSON.parse(xhr.responseText);
          cb();
        } else {
          console.error('Unable to fetch hydrator map');
        }
      }
    }
    xhr.open('GET', '/static/data/map.json');
    xhr.send();
  }

  function decode(response) {
    return (response && typeof response.value === 'object' && response.length) ? hydrate(response.value) : '';
  }

  function generateEntity(modelName, data = {}) {
    var model = map[modelName],
      defTypes, obj, k, n, i;

    if (typeof data !== 'object') {
      return console.error('[hydratorService] Argument `data` must be object.');
    }
    if (!model) {
      return console.error('[hydratorService] Model name `' + modelName + '` is not found in map.json');
    }

    obj = getEmpty(model);
    for (k in data) {
      if (obj[k] === undefined) {
        // return console.error('[hydratorService] Undefined key `' + k + '` in model `' + modelName + '`');
      }
      if (data[k] !== undefined && data[k] !== null) {
        obj[k] = data[k];
      }
    }

    // generateEntity вложеных объектов
    defTypes = ['real', 'integer', 'string'];
    dataLoop:
      for (k in data) {
        for (n in model) {
          if (k === model[n].name) {
            if (!model[n].relatedType || defTypes.indexOf(model[n].relatedType) !== -1) {
              continue dataLoop;
            }

            // sequence
            if (model[n].type === 'sequence') {
              obj[k] = generateEntity(model[n].relatedType, data[k]);
              continue dataLoop;
            }

            // set
            if (data[k] && data[k].length) {
              for (i = 0; i < data[k].length; i++) {
                obj[k][i] = generateEntity(model[n].relatedType, data[k][i]);
              }
            }
            continue dataLoop;
          }
        }
        // return console.error('[hydratorService] Undefined key `' + k + '` in model `' + modelName + '`');
      }

    return obj;
  }

  function getEmpty(model) {
    var result = {},
      len = model.length,
      i;

    for (i = 0; i < len; i++) {
      switch (model[i].type) {
        case 'set':
          result[model[i].name] = [];
          break;

        case 'integer':
          result[model[i].name] = 0;
          break;

        case 'sequenceof':
          result[model[i].name] = [];
          break;

        case 'boolean':
          result[model[i].name] = false;
          break;

        default:
          result[model[i].name] = {type: 'atom', value: 'undefined'};
      }
    }

    return result;
  }

  function hydrateBoolean(data) {
    if (typeof data === 'boolean') {
      return data;
    } else {
      console.error('hydrateBoolean error');
    }
  }

  function hydrateInteger(data) {
    if (Number.isInteger(data)) {
      return data;
    } else {
      console.error('hydrateInteger error');
    }
  }

  function hydrateReal(data) {
    return data;
  }

  function hydrateString(data) {
    let value;
    if (typeof data === 'string') {
      value = data;
    } else if (typeof data === 'object' && data instanceof Array) {
      if (data.length === 0) {
        value = undefined;
      } else {
        value = String.fromCharCode(...data);
      }
    } else if (typeof data === 'object' && data.value instanceof Array) {
      value = utf8ArrayToStr(data.value);
    } else {
      console.error('hydrateString error');
    }
    return value;
  }

  function hydrate(message) {
    if (!message || message === 'undefined') {
      // console.log('[HYDRATOR] Empty entity');
      return null;
    }

    const result = {
      modelName: message[0].value
    };
    const modelName = message[0].value;
    const modelFields = map[modelName];

    // console.log('[HYDRATOR] Hydrating message ' + modelName + ' with value ' + JSON.stringify(message));

    if (!modelFields) {
      console.error('[Hydrator] Unknown model ' + modelName);
    }

    for (let x = 0; x < modelFields.length; x++) {
      const field = modelFields[x];
      const data = message[x + 1];

      if (data === undefined || data.value === 'undefined') {
        result[field.name] = null;
        continue;
      }

      switch (field.type) {
        case 'set':
        case 'sequenceof':
          let sequenceData = data;
          if (sequenceData[`'${field.relatedType}'`]) {
            sequenceData = sequenceData[`'${field.relatedType}'`];
          }
          // console.log('[HYDRATOR] Decode sequence ' + JSON.stringify(field) + ' of: ', data);
          if (sequenceData.constructor === Array) {
            result[field.name] = [];

            for (let i = 0; i < sequenceData.length; i++) {
              if (field.relatedType === 'string') {
                result[field.name].push(hydrateString(sequenceData[i]));
              } else if (field.relatedType === 'real') {
                result[field.name].push(hydrateReal(sequenceData[i]));
              } else if (field.relatedType === 'integer') {
                result[field.name].push(hydrateInteger(sequenceData[i]));
              } else if (field.relatedType === 'boolean') {
                result[field.name].push(hydrateBoolean(sequenceData[i]));
              } else {
                result[field.name].push(hydrate(sequenceData[i].value));
              }
            }
          } else if (sequenceData === "\u0000\u0000" || sequenceData === "") {
            result[field.name] = [null, null];
          } else {
            result[field.name] = [];
            // return console.error('[hydratorService] Incorrect field `' + field.name +
            //   '` format: expect "Array", received: ' + typeof sequenceData);
          }
          break;

        case 'sequence':
          result[field.name] = hydrate(data.value);
          // console.log('[HYDRATOR] Set sequence ' + field.name + ' to value ', result[field.name]);
          break;

        case 'integer':
          result[field.name] = hydrateInteger(data);
          break;

        case 'real':
          result[field.name] = hydrateReal(data);
          break;

        case 'string':
          result[field.name] = hydrateString(data);
          break;

        case 'boolean':
          result[field.name] = hydrateBoolean(data);
          break;

        case 'generalizedtime':
          result[field.name] = moment.utc(data, 'YYYYMMDDHHmmss.SSS[Z]')
            .local()
            .format('YYYYMMDDHHmmss.SSS[Z]');
          break;
      }
    }

    return result;
  }

  function encode(sequenceName, obj) {
    // console.log('Encoding ' + JSON.stringify(sequenceName) + ' with obj ' + JSON.stringify(obj));

    var data = [],
      sequenceMap = map[sequenceName],
      fieldName, flen, f;

    if (!sequenceMap) {
      console.error('Unknown entity ' + JSON.stringify(sequenceName));
    }

    // if (objectSize(obj) !== objectSize(sequenceMap)) {
    //   return console.error('[hydratorService] Error fields count not equal.');
    // }

    data.push(Erl.atom(sequenceName));

    for (f = 0, flen = sequenceMap.length; f < flen; f++) {
      fieldName = sequenceMap[f].name;

      if (obj[fieldName] === undefined) {
        // return console.error('[hydratorService] Field `' + fieldName + '` not exist.');
      }

      data.push(convert(sequenceMap[f], obj[fieldName]));
    }

    function convert(fieldMap, value) {
      // console.log('Converting fieldMap ' + fieldMap.name + ' with value ' + JSON.stringify(value));

      var result = [],
        itemMap, len, i, j;

      switch (fieldMap.type) {
        case 'set':
        case 'sequenceof':
          if (value && value.constructor === Array) {
            itemMap = { type: fieldMap.relatedType };

            if (map.hasOwnProperty(fieldMap.relatedType)) {
              for (i = 0, len = value.length; i < len; i++) {
                result.push(encode(fieldMap.relatedType, value[i]));
              }
            } else {
              for (j = 0, len = value.length; j < len; j++) {
                result.push(convert(itemMap, value[j]));
              }
            }
          } else if (value && value.type == 'atom' && value.value == 'undefined') {
            result.push(Erl.atom('undefined'));
          } else if (value) {
            return console.error('[hydratorService] Incorrect field `' + fieldMap.name +
              '` format: expect "Array", received: ' + typeof value);
          }
          return result;

        case 'integer':
          return (typeof value === 'string') ? parseInt(value) : value;

        case 'real':
          return value;

        case 'string':
          if (typeof value === 'string') {
            return Erl.binary(utf8ToByteArray(value));
          } else if (typeof value === 'object' && value.type === 'atom') {
            return Erl.atom(value.value);
          }
          return value;

        case 'binary':
          if (value instanceof Uint8Array) {
            return Erl.binary(Array.from(value));
          } else if (value instanceof Array) {
            return Erl.binary(value);
          }
          throw new Error("Unsupported binary data type: " + Erl.getClassName(value));

        case 'boolean':
          return value;

        case 'generalizedtime':
          if (typeof value === 'object' && value.type === 'atom') {
            return Erl.atom(value.value);
          }
          return moment(value, 'YYYYMMDDHHmmss.SSS[Z]').utc().format('YYYYMMDDHHmmss.SSS[Z]');

        case 'sequence':
          if (typeof value === 'object' && value.type === 'atom') {
            return Erl.atom(value.value);
          }
          return encode(fieldMap.relatedType, value);
      }
    }

    return Erl.tuple.apply(null, data);
  }

  function objectSize(obj) {
    let count;

    if (typeof obj === 'object') {
      count = Object.keys(obj).length;
    }

    return count;
  }


  function utf8ToByteArray(str) {
    const utf8 = [];
    for (let i = 0; i < str.length; i++) {
      let charCode = str.charCodeAt(i);
      if (charCode < 0x80) {
        utf8.push(charCode);
      }
      else if (charCode < 0x800) {
        utf8.push(0xc0 | (charCode >> 6),
          0x80 | (charCode & 0x3f));
      }
      else if (charCode < 0xd800 || charCode >= 0xe000) {
        utf8.push(0xe0 | (charCode >> 12),
          0x80 | ((charCode >> 6) & 0x3f),
          0x80 | (charCode & 0x3f));
      }
      // surrogate pair
      else {
        i++;
        // UTF-16 encodes 0x10000-0x10FFFF by
        // subtracting 0x10000 and splitting the
        // 20 bits of 0x0-0xFFFFF into two halves
        charCode = 0x10000 + (((charCode & 0x3ff) << 10)
          | (str.charCodeAt(i) & 0x3ff));
        utf8.push(0xf0 | (charCode >> 18),
          0x80 | ((charCode >> 12) & 0x3f),
          0x80 | ((charCode >> 6) & 0x3f),
          0x80 | (charCode & 0x3f));
      }
    }
    return utf8;
  }

  function utf8ArrayToStr(data) {
    let str = '';

    for (let i = 0; i < data.length; i++) {
      const value = data[i];

      if (value < 0x80) {
        str += String.fromCharCode(value);
      } else if (value > 0xBF && value < 0xE0) {
        str += String.fromCharCode((value & 0x1F) << 6 | data[i + 1] & 0x3F);
        i += 1;
      } else if (value > 0xDF && value < 0xF0) {
        str += String.fromCharCode((value & 0x0F) << 12 | (data[i + 1] & 0x3F) << 6 | data[i + 2] & 0x3F);
        i += 2;
      } else {
        // surrogate pair
        const charCode = ((value & 0x07) << 18 | (data[i + 1] & 0x3F) << 12 | (data[i + 2] & 0x3F) << 6 |
          data[i + 3] & 0x3F) - 0x010000;

        str += String.fromCharCode(charCode >> 10 | 0xD800, charCode & 0x03FF | 0xDC00);
        i += 3;
      }
    }

    return str;
  }
})();
