var EventController = (function(){
  "use strict";
  var ws, Callbacks = {}, state, Typing = {};

  return {
    setWebsocket: setWebsocket,
    onMessage: onMessage,
    sendRequest: sendRequest,
    sendFiles: sendFiles,
    genRef: generateRef
  };

  function setWebsocket(smoothieWs) {
    ws = smoothieWs;
  }

  function onMessage(message) {
    if(message instanceof ErlTuple){
      if(message.value[0] instanceof ErlAtom) {
        console.log('Получено сообщение "%s"', message.value[0].value, message);
        var responceData = Hydrator.decode(message);
        if(responceData.ref && Callbacks.hasOwnProperty(responceData.ref)){
          console.log('Выполнить Callback для ref:%d' , responceData.ref);
          console.log(Callbacks);
          if(Callbacks[responceData.ref]) {

            Callbacks[responceData.ref](message.value[0].value, responceData);
            delete Callbacks[responceData.ref];
          }
        }
//        app.dispatchEvent(new CustomEvent('ws:' + message.value[0].value, { 'detail': responceData }));
      }else {
        console.log("Incorrect message format!");
      }
    } else {
      console.log("Incorrect message format!");
    }
  }

  function sendRequest(action, data, cb){
    if(data.ref){
      if(cb && cb instanceof Function){
        Callbacks[data.ref] = cb;
      }
    }
    ws.send(Hydrator.encode(action, data));
  }

  function sendFiles(options, files, completeCb, progressCb, errorCb) {
    Smoothie.sendFiles(options, files, completeCb, progressCb, errorCb);
  }

  function generateRef() {
    return Number(('' + Math.floor(Math.random() * 10e2)) + ('' + Date.now()));
  }

})();