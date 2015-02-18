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
    
    var json = JSON.stringify({"type":"setup","arrity":$scope.arrity});
    var socket= new WebSocket('ws://localhost:8080/websocket');
    
    // the onopen event hook triggers this
    socket.onopen = function() {
		socket.send(json);
		// this is in the event of a reply coming back
		socket.onmessage = function(evt) {
		// parse the data from the reply json object
		var data = JSON.parse(evt.data);;
		// update the grid visulaisation
		$scope.updateGrid($scope.arrity);
		};
	};
	
	// this is in a seperate function so it can be triggered when the input field is updated
	$scope.updateGrid = function() {
		// compile a 'setup' typed JSON
		var update_json = JSON.stringify({"type":"setup","arrity":$scope.arrity});
		socket.send(update_json);
		socket.onmessage = function(evt) {
			var data = JSON.parse(evt.data);
			// draw the grid
			setup_grid(data.rows,data.tileSize,$scope.gridScale);
		};
	};
	
	$scope.updateZoom = function() {
		//setup_grid($scope.arrity,50,$scope.gridScale);
		$scope.updateGrid();
		};
		
	$scope.updateSwarm = function() {
		var swarm_json = JSON.stringify({"type":"swarm","size":$scope.swarmSize});
		socket.send(swarm_json);
		socket.onmessage = function(sw) {
          // draw the swarm 
          swarm_data = JSON.parse(sw.data);
          //console.log(swarm_data);
          draw_circles(swarm_data,$scope.gridScale);
        };    
	};
		
	$scope.update = function() {
		var inspector = angular.element("#inspector");
		console.log(inspector);
		var startjson = JSON.stringify({"type":"start"});
		socket.send(startjson);
		socket.onmessage = function (x) {;}
		inspector.value ="";
		var dummy_json = JSON.stringify({"type":"report"});
		//This sends the "update" message to the socket every 1000ms
		// and updates the circles with the recived data
		setInterval(function() {doUpdate()},300);
		function doUpdate() {
			var inspectList =[];
			socket.send(dummy_json);
			socket.onmessage = function(evt) {
				var report_json = JSON.parse(evt.data);
				update_circles(report_json,$scope.gridScale,$scope.swarmSize);
				for (var i = 0; i<inspectList.length; i++) {
					var element = inspectList[i];
      inspector.value = "id: "+element.__data__.id+", Pos: "+element.__data__.x+","+element.__data__.y+","+element.__data__.viewer+"\n" + inspector.value;
    }
    };
  };
};

  }]);//end of 'PanelController'  
  
})();
