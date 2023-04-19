var FrameworkPipeline = require('./FrameworkPipeline');

module.exports = function(params){
  const commands = [];

  this.test = function(t, callback) {
	  const p = FrameworkPipeline(this, t);

    for (let x in commands) {
      p.addCommand(commands[x].name, commands[x].handler)
    }

    return p
  };

  this.addCommand = function(name, handler){
    commands.push({name: name, handler: handler})
  };

  return {
    params    : params,
    test      : test,
    addCommand: this.addCommand
  }
};
