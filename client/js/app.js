(function(){
  var app = angular.module('swarm',[]);
  



  app.controller('SwarmController', function(){
    
  
  });
  app.controller('PanelController', function(){
  
  function setup_grid(arrity,tileSize,gridScale) {
  arrity = parseInt(arrity);
  tileSize = parseInt(tileSize);
  gridScale = parseInt(gridScale);
  var list =[];
  for (var i = 0; i <= arrity; i++) {
    list.push(i*tileSize);
  };
  draw_hlines(arrity,tileSize,gridScale,list);
  draw_vlines(arrity,tileSize,gridScale,list);
};
    
    // compile a JSON object
    var json = JSON.stringify({"type":"setup","arrity":arrity});
    
    //establish a socket
    var socket= new WebSocket('ws://localhost:8080/websocket');
    
    // the onopen evnt hook triggers this
    socket.onopen = function() {
    socket.send(json);
    // this is the event of a reply coming back
    socket.onmessage = function(evt) {
      // data from the reply
      var data = JSON.parse(evt.data);
      console.log(evt);
      
      setup_grid(arrity,size,gridScale);
      
      
    };
  };
  
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

  //var arrity = saneDefaultGrid.arrity;
  //var gridScale = saneDefaultGrid.gridScale;
  //var swarmSize = saneDefaultGrid.swamSize;
  //var popSize = saneDefaultGrid.popSize;
  

  
})();
