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
    $scope.swarmSizeOpts = [1,2,5,10,20,30,40,50,100];
    $scope.swarmSize = $scope.swarmSizeOpts[2];
    $scope.popSizeOpts = [1,2,5,10,20,30,40,50,100];
    $scope.popSize = $scope.popSizeOpts[4];
    $scope.runningFlag=false;

    var fourblocks_map = "...................................................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiii.........iiiiii..iiiiii..........iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiii..........iiiiiiiiiiiiiiii..........iiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...................................................";
    
    var maze_map = "...................................................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii.....................ii.....................ii..ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii..ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii..ii.ii......ii......................ii......ii.ii..ii.ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii.ii..ii.ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.iiiii..ii.ii.ii...............ii...............ii.iiiii..iiiii.ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii.ii.ii..iiiii.ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii.ii.ii..ii.ii.iiiii..ii..................ii..ii.ii.ii.ii..ii.ii.iiiii.iiiiiiiiiiiiiiiiiiiiiiii.ii.ii.ii.ii..ii.ii.ii.ii.iiiiiiiiiiiiiiiiiiiiiiii.ii.ii.ii.ii..ii.ii.ii.ii.ii.........ii.........ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.iiiiiiiiiiiiiiiiii.ii.iiiii.ii.ii..ii.ii.ii.ii.ii.iiiiiiiiiiiiiiiiii.ii.iiiii.ii.ii..ii.ii.ii.ii.ii.ii..............ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.iiiii.iiiii.iiiiiiiiiiiiiiiiii.iiiii.iiiii.ii..ii.iiiii.iiiii.iiiiiiiiiiiiiiiiii.iiiii.iiiii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii.iiiiiiiiiiii.ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.ii..............ii.ii.ii.ii.ii.ii..ii.ii.ii.ii.ii.iiiiiiiiiiiiiiiiii.ii.iiiii.ii.ii..ii.ii.ii.ii.ii.iiiiiiiiiiiiiiiiii.ii.iiiii.ii.ii..ii.ii.iiiii.ii..........ii........ii.ii.ii.ii.ii..ii.ii.iiiii.iiiiiiiiiiiiiiiiiiiiiiii.ii.ii.ii.ii..ii.ii.ii.ii.iiiiiiiiiiiiiiiiiiiiiiii.ii.ii.ii.ii..ii.ii.ii.ii..ii..................ii..ii.ii.ii.ii..ii.ii.ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii.ii.ii..ii.ii.ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii.ii.ii..iiiii.ii................ii..............ii.ii.ii..iiiii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii.ii..ii.ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.iiiii..ii.ii......ii........................ii....iiiii..ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii..ii.iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.ii..ii......................ii....................ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...................................................";
    $scope.simpleObString=fourblocks_map;
    $scope.simpleObArray =[];
    //for the 50x50 obarray
    for (var i=0;i<$scope.simpleObString.length;i++) {
		if ($scope.simpleObString[i] !== "i") {
			$scope.simpleObArray.push(i);
			}
		}; 
    socket = new WebSocket('ws://localhost:8080/websocket');
    var json = JSON.stringify({"type":"setup","arrity":$scope.arrity,"swarmSize":$scope.swarmSize,"popSize":$scope.popSize,"obArray":$scope.simpleObArray});
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
      //console.log(gridInfo);
      setup_grid(gridInfo.rows,gridInfo.tileSize,$scope.gridScale,gridInfo.obs);
      draw_circles(data,$scope.gridScale,$scope);
      };
    };
    
    // Yes it's a bit repetitive
    $scope.update = function() {
    d3.select("svg").html('<defs><pattern id="grass" x="0" y="0" height="64" width="64" patternUnits="userSpaceOnUse" ><image height="64" width="64" xlink:href="img/grasstile.jpg"></image></pattern><pattern id="brick" x="0" y="0" height="50" width="50" patternUnits="userSpaceOnUse" ><image height="50" width="50" xlink:href="img/bricktile.jpg"></image></pattern></defs>');
    var new_json = JSON.stringify({"type":"setup","arrity":$scope.arrity,"swarmSize":$scope.swarmSize,"popSize":$scope.popSize,"obArray":$scope.simpleObArray});
    if (socket.readyState != 0) {
    socket.send(new_json);
  };
    socket.onmessage = function(evt) {
    // the onopen event hook triggers this
      var data = JSON.parse(evt.data);;
      // update the grid visulaisation
      // draw the grid
      var gridInfo = data.pop();
      //console.log(gridInfo);
      setup_grid(gridInfo.rows,gridInfo.tileSize,$scope.gridScale,gridInfo.obs);
      draw_circles(data,$scope.gridScale,$scope);
      };
		};
	

	$scope.start = function() {
		$scope.runningFlag=!$scope.runningFlag;
		$scope.startbtn = angular.element("#start-btn");

		var startjson = JSON.stringify({"type":"start"});
		socket.send(startjson);
		socket.onmessage = function (x) {;};
		var dummy_json = JSON.stringify({"type":"report"});
		//This sends the "update" message to the socket every X ms
		// and updates the circles with the recived data
		setInterval(function() {doUpdate()},290);
		function doUpdate() {
			socket.send(dummy_json);
			socket.onmessage = function(evt) {
				var report_json = JSON.parse(evt.data);
         //console.log(report_json);
				update_circles(report_json,$scope.gridScale,$scope.swarmSize,$scope);
        $scope.realInspectList =[];
        for (var i =0;i<$scope.inspectList.length;i++){
			$scope.realInspectList.push(report_json[(report_json.map(function(e) { return e.id; }).indexOf($scope.inspectList[i].id))]);
		};
		update_web($scope.realInspectList,$scope.gridScale,$scope);
		$scope.$apply();
	};  
    };
  };
  
  $scope.pause = function() {
    $scope.runningFlag=!$scope.runningFlag;
    var pausejson = JSON.stringify({"type":"pause"});
    socket.send(pausejson);
    socket.onmessage = function (x) {;};
  };
  
  }]);//end of 'PanelController'  
  
})();
