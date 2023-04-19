var FrameworkPipeline = require('./core/FrameworkPipeline')

module.exports = function(params){
  var commands = []

  this.test = function(t, callback) {
    var p = FrameworkPipeline(this, t)

    for (var x in commands) {
      p.addCommand(commands[x].name, commands[x].handler)
    }

    return p
  }

  this.addCommand = function(name, handler){
    commands.push({name: name, handler: handler})
  }

  return {
    params    : params,
    test      : test,
    addCommand: addCommand
  }
}
