const execSync = require('child_process').execSync;

module.exports = (function(){
  const clearDB = function(dbName){
    // console.log("# Clear DB");
    execSync('mongo ' + dbName + ' --eval "db.dropDatabase()"', true);
    // console.log('#> mongo ' + dbName + ' --eval "db.dropDatabase()"');
  };
	
	const clearElasticDB = function(host, port, indexName, doc){
    // console.log("# Clear ElasticSearch DB");
		const query = `curl -XPOST '${host}:${port}/${indexName}/${doc}/_delete_by_query?conflicts=proceed&pretty' -H 'Content-Type: application/json' -d '{"query": {"match_all": {}}}'`;
    // execSync(query, {stdio: 'ignore'});
    execSync(query, true);
  };
	
	const apply = function(dumpsDir){
    // console.log("# Add fixtures");
    execSync('mongorestore --quiet --drop ' + dumpsDir + '/', true);
    // console.log('#> mongorestore --quiet --drop ' + dumpsDir + '/');
  };

  return {
    clearDB: clearDB,
    clearElasticDB: clearElasticDB,
    apply: apply
  }
})();
