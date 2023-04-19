define(function(){
  let ws = null;

  return {
    connect: (onMessage, onOpen = null) => {
      let hydratorReady = false;

      return ws = Smoothie.connect({
        http: {
          path: '/ws?token=' + getParameterByName('token') + '&version=18-01-2017'
        },
        protocol: 'bert',
        onOpen: onOpen,
        onMessage: data => {
          // console.log("Got message:", Erl.toString(data));

          if (hydratorReady) {
            onMessage(Hydrator.decode(data));
          } else if (data.value && data.value[0].value == 'AuthResp') {
            Hydrator.fetchMap(() => {
              hydratorReady = true;
              onMessage({auth: data.value[1]});
            });
          }
        },
      });
    },
    send: (name, data) => {
      console.log('Sending', name, data);
      ws.send(Hydrator.encode(name, Hydrator.generateEntity(name, data)));
    }
  }
});

function getParameterByName(name) {
  name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
  var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
    results = regex.exec(location.search);
  return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
}
