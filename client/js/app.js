(function(){
  var app = angular.module('swarm',[]);
  



  app.controller('SwarmController', function(){
    
  
  });
  app.controller('PanelController', ['$scope' ,function($scope){
    $scope.arrity = 5;
    // compile a JSON object
    
    console.log($scope.arrity);
    var json = JSON.stringify({"type":"setup","arrity":$scope.arrity});
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
    };
      
      
 
  };
  
  }]);
  

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

  
  //var gridScale = saneDefaultGrid.gridScale;
  //var swarmSize = saneDefaultGrid.swamSize;
  //var popSize = saneDefaultGrid.popSize;
  

  
})();
