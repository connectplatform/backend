(function(){
  require(['data', 'entity', 'logger', 'wsHelper', 'httpHelper', 'entityHelper'], function(data, entity, logger, wsHelper, httpHelper, entityHelper) {
    this.dataModule = data
    this.entityModule = entity
    this.loggerModule = logger
    this.wsHelper = wsHelper
    this.httpHelper = httpHelper
    this.entityHelper = entityHelper

    init()
  })

  var init = function(){
    var self = this

    $.getJSON("/static/data/map.json", function(json) {
      self.map = json
      self.entityHelper.setMap(json)
//      buildData($('#WS .messageName').val(), $('#WS .messageData'))
      buildData($('#HTTP .messageName').val(), $('#HTTP .messageData'))
    });

    $("#WS .send").on("click", function(){
      var messageName = $('#WS .messageName').val()
      var entity = self.entityHelper.get(messageName)

    })
  }

  var buildData = function(messageName, selector){
    var self = this

    var drowField = function(data) {
      return '<td>' + data.name + ': </td><td><input type="text" /></td>';
    }

    var drowRelated = function(fieldName, messageName){
      var res = '<div class="related">'
      res += '<div class="relatedFieldName">' + fieldName + '</div>'
      res += '<table class="relatedData">'

      var entity = self.map[messageName]
      for (var i = 0; i < (entity.length ) ; i++) {
        res += '<tr>'
        if (entity[i].relatedType) {
          res += drowRelated(entity[i].name, entity[i].relatedType)
        } else {
          res += drowField(entity[i])
        }
        res += '</tr>'
      }
      res += '</div>'

      res += '</table>'

      return res
    }

    var drow = function(messageName){
      var res = '<table>'
      var entity = self.map[messageName]
      for (var i = 0; i < (entity.length ) ; i++) {
        res += '<tr>'
        if(entity[i].relatedType){
          res += drowRelated(entity[i].name, entity[i].relatedType)
        }else{
          res += drowField(entity[i])
        }
        res += '</tr>'
      }

      res += '</table>'
      return res
    }

    $(selector).html(drow(messageName))
  }

  return {

  }
})()