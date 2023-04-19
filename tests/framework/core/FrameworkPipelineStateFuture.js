module.exports = function(name, fun) {
  return {
    type: 'FrameworkPipelineStateFuture',
    fun: fun,
    name: name
  }
};
