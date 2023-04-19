(function(document, window) {
  'use strict';

  var app = {};
  app.config = {
       "name": "Radius",
       "cloudServer": {
         "http": {
           "protocol":     "ws://",
           "host":         "cloud.roster.ck.ua",
           "port":         5010,
           "path":         "/upload"
         },
         "protocol": "relay"
       },
       "idServer": {
//          "url": "id.roster.ck.ua",
         "url": "localhost",
         "port": 3050
       },
       "imServer": {
//          "url": "im.roster.ck.ua",
         "url": "localhost",
         "port": 5000
       }
     }
  app.token = getParameterByName("token");

  function bootApp() {
    app.evc = EventController;
    //connect to websocket
    var smoothieOptions = {
      http: {
        host: app.config.imServer.url,
        port: app.config.imServer.port,
        path: "/ws"
      },
      queryParams: {token: app.token},
      protocol:  "bert"
    };

    smoothieOptions.onOpen = function(){
      console.log('Connect to smoothie websocket!');
    };
    smoothieOptions.onMessage = function(data) {
      app.evc.onMessage(data);
    };
    smoothieOptions.onDisconnect = function() {
      console.log('Disconnect smoothie websocket!');
    };
    smoothieOptions.onClose = function() {
      console.log('Smoothie websocket connection closed!');
    };

    app.evc.setWebsocket(Smoothie.connect(smoothieOptions));
  }

  bootApp();
  window.app = app;
})(document, window);