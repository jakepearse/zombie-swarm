(function(){
  var app = angular.module('swarm',[]);
  



  app.controller('SwarmController', function(){
    
  });//end of 'SwarmController'
  
  app.controller('PanelController', ['$scope' ,function($scope){
    
    // Inital values for the form inputs
    $scope.arrity = 5;
    $scope.gridScale = 4;
    $scope.swarmSize = 50;
    $scope.popSize = 20;
    socket = new WebSocket('ws://localhost:8080/websocket');
    var json = JSON.stringify({"type":"setup","arrity":$scope.arrity,"swarmSize":$scope.swarmSize,"popSize":$scope.popSize});
    $scope.inspectList = [];
    socket.onopen = function() {
      socket.send(json);
      // this is in the event of a reply coming back
      socket.onmessage = function(evt) {
      // parse the data from the reply json object
      var data = JSON.parse(evt.data);
      // update the grid visulaisation
      // draw the grid
      var gridInfo = data.pop();
      setup_grid(gridInfo.rows,gridInfo.tileSize,$scope.gridScale);
      draw_circles(data,$scope.gridScale,$scope);
      };
    };
    
    // Yes it's a bit repetitive
    $scope.update = function() {
    d3.select("svg").html("");
    var new_json = JSON.stringify({"type":"setup","arrity":$scope.arrity,"swarmSize":$scope.swarmSize,"popSize":$scope.popSize});
    if (socket.readyState != 0) {
    socket.send(new_json);
  };
    socket.onmessage = function(evt) {
    // the onopen event hook triggers this
      var data = JSON.parse(evt.data);;
      // update the grid visulaisation
      // draw the grid
      var gridInfo = data.pop();
      setup_grid(gridInfo.rows,gridInfo.tileSize,$scope.gridScale);
      draw_circles(data,$scope.gridScale,$scope);
      };
		};
	

	$scope.start = function() {
		$scope.inspector = angular.element("#inspector");
		var startjson = JSON.stringify({"type":"start"});
		socket.send(startjson);
		socket.onmessage = function (x) {;}
		//inspector.value ="";
		var dummy_json = JSON.stringify({"type":"report"});
		//This sends the "update" message to the socket every 1000ms
		// and updates the circles with the recived data
		setInterval(function() {doUpdate()},300);
		function doUpdate() {
			socket.send(dummy_json);
			socket.onmessage = function(evt) {
				var report_json = JSON.parse(evt.data);
				update_circles(report_json,$scope.gridScale,$scope.swarmSize);
        
    };
  };
};
	$scope.update();
  }]);//end of 'PanelController'  
  
})();
