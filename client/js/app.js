(function(){
  var app = angular.module('swarm',[]);
  



  app.controller('SwarmController', function(){
    
  });//end of 'SwarmController'
  
  app.controller('PanelController', ['$scope' ,function($scope){
    
    // Inital values for the form inputs
    $scope.arrityOpts = [3,4,5,6,7,8,9,10];
    $scope.arrity = $scope.arrityOpts[2];
    $scope.gridScaleOpts = [1,2,3,4,5,6,7,8,9,10];
    $scope.gridScale = $scope.gridScaleOpts[5];
    $scope.swarmSizeOpts = [5,10,20,30,40,50,100,200,500,1000];
    $scope.swarmSize = $scope.swarmSizeOpts[2];
    $scope.popSizeOpts = [5,10,20,30,40,50,60];
    $scope.popSize = $scope.popSizeOpts[1];
    $scope.runningFlag=false;
    
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
		$scope.runningFlag=!$scope.runningFlag;
		//console.log($scope.runningFlag);
		$scope.startbtn = angular.element("#start-btn");
		$scope.startbtn.html("Stop");
		//console.log($scope.startbtn);
		var startjson = JSON.stringify({"type":"start"});
		socket.send(startjson);
		socket.onmessage = function (x) {;};
		var dummy_json = JSON.stringify({"type":"report"});
		//This sends the "update" message to the socket every X ms
		// and updates the circles with the recived data
		setInterval(function() {doUpdate()},300);
		function doUpdate() {
			socket.send(dummy_json);
			socket.onmessage = function(evt) {
				var report_json = JSON.parse(evt.data);
				update_circles(report_json,$scope.gridScale,$scope.swarmSize);
        $scope.realInspectList =[];
        for (var i =0;i<$scope.inspectList.length;i++){
			$scope.realInspectList.push(report_json[(report_json.map(function(e) { return e.id; }).indexOf($scope.inspectList[i].id))]);
		};
		//console.log($scope.realInspectList);
		$scope.$apply();
	};  
    };
  };
  }]);//end of 'PanelController'  
  
})();
