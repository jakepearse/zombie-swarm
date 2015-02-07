(function(){
  var app = angular.module('swarm',[]);

  app.controller('SwarmController', function(){
  
  });
  app.controller('PanelController', function(){
      
  });
  

  // this stuff declared at the outer lexical scope is accessible
  // inside the inner functions ...
  // I'm not sure wheter this is a good idea
  var saneDefaultGrid = 
    {
      type:'setup',
      arrity:5,
      tileSize:50,
      gridScale:2,
      swarmSize:50,
      popSize:50
    };
	
	var arrity = saneDefaultGrid.arrity;
	var gridScale = saneDefaultGrid.gridScale;
	var swarmSize = saneDefaultGrid.swamSize;
	var popSize = saneDefaultGrid.popSize;
})();
