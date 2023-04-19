var IdApi = function (config, Erl, Hydrator) {

    var apiUrl = 'http://' + config.idServer.url + ':' + config.idServer.port + '/api';

    var v1 = function (msgName, msgData, token, cb, errCb) {
        var path = apiUrl + '/v1';
        if(token){
            path += '?token=' + token;
        }
        postBert(path, msgName, msgData, cb, errCb);
    };

    function postBert (path, msgName, msgData, cb, errCb) {
        var params = {
            method: "POST",
            processData: false,
            dataType: 'binary',
            responseType:'arraybuffer',
            headers: {
                "Content-Type": "application/octet-stream"
            },
            data: Erl.encode(Hydrator.encode(msgName, msgData))
        };

        $.ajax(path, params).done(function(response) {
            var data = Hydrator.decode(Erl.decode(response));
            cb && cb(data);
        }).fail(function (response) {
            console.warn('IdApi.postBert Error: ', response);
            errCb && errCb(response);
        });
    }

    function getCountry(cb, errCb){
        $.getJSON("http://freegeoip.net/json/")
          .done(function( json ) {
              cb && cb(json);
          })
          .fail(function( jqxhr, textStatus, error ) {
              errCb && errCb(jqxhr, textStatus, error);
          });
    }

    function getLocation(cb, errCb){
        navigator.geolocation.getCurrentPosition(function (data) {
            console.log('CurrentLocation: ', data);
            cb && cb({latitude:data.coords.latitude, longitude:data.coords.longitude});
        });
    }

    return {
        v1: v1,
        getLocation: getLocation,
        getCountry: getCountry
    }

};