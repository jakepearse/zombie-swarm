(function(){
  var app = angular.module('swarm',[]);
  



  app.controller('SwarmController', function(){
    
  });//end of 'SwarmController'
  
  app.controller('PanelController', ['$scope' ,function($scope){
    
    // Inital values for the form inputs
   // $scope.arrityOpts = [3,4,5,6,7,8,9,10];
   // $scope.arrity = $scope.arrityOpts[7];
    $scope.gridScaleOpts = [1,2,3,4,5,6,7,8,9,10];
    $scope.gridScale = $scope.gridScaleOpts[8];
    $scope.swarmSizeOpts = [0,1,2,5,10,20,30,40,50,100];
    $scope.swarmSize = $scope.swarmSizeOpts[2];
    $scope.popSizeOpts = [1,2,5,10,20,30,40,50,100,250];
    $scope.popSize = $scope.popSizeOpts[8];
    $scope.foodAmountOpts = [5,10,20,30];
    $scope.foodAmount = $scope.foodAmountOpts[1];
    $scope.runningFlag=false;
    $scope.contolsFlag=false;
    $scope.gridMaps = [
      {"tag": "5-blocks", "arrity":5, "map":"......................................................................................................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....ii..........iii...ii...ii......ii...iii...iiii....ii..........iii...ii...ii......ii...iii...iiii....ii..........iii...ii...ii......ii...iii...iiii....iiiiiiiiiiiiiii...ii...iiiii...iiiiiiiiiiiiiii....iiiiiiiiiiiiiii...ii...iiiii...iiiiiiiiiiiiiii....ii...iiii...iii...ii...iiiii...iiiiiiiiiiiiiii....ii...iiii.........ii......ii...ii...iii...iiii....ii...iiii.........ii......ii...ii...iii...iiii....ii...iiii.........ii......ii...ii...iii...iiii....ii...iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....ii.........iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....ii.........iiiiiiiiiiiiiiiiiiiiii...iii...iiii....ii.........iiiiiiiiiiiiiiiiiiiiii...iii...iiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...iii...iiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....ii................iii........iiii...........ii....ii................iii........iiii...........ii....ii................iii........iiii...........ii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiii......................iiiiiiiiii....iiiiiiiiiiiiii......................iiiiiiiiii....iiiiiiiiiiiiii......................iiiii...ii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...ii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...ii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...ii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...ii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...iiii...ii....iii...ii...ii...iii...iii...iiiiii...iiii...ii....iii...ii...ii...iii...iii...iiiiii...iiii...ii....iii...ii...ii...iii...iii...iiiiiiiiiiiii...ii....iiiiiiiiiiiiiiiiiiiiiiiii...iiiiiiiiiiiii...ii....iiiiiiiiiiiiiiiiiiiiiiiii...iiiiiiiiiiiii...ii....iiiiiiiiiiiiiiiiiiiiiiiii...iiiiiiiiiiiii...ii....iii................iiiiii...iiii............ii....iii................iiiiii...iiii............ii....iii................iiiiii...iiii............ii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii......................................................................................................"},
      {"tag": "5-cardinal","arrity":5 ,"map": "......................................................................................................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iii..iiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiii..iii....iii...iiiiiiiiiiiiiiii..iiiiiiiiiiiiiiii...iii....iiii...iiiiiiiiiiiiiii..iiiiiiiiiiiiiii...iiii....iiiii...iiiiiiiiiiiiii..iiiiiiiiiiiiii...iiiii....iiiiii...iiiiiiiiiiiii..iiiiiiiiiiiii...iiiiii....iiiiiii...iiiiiiiiiiii..iiiiiiiiiiii...iiiiiii....iiiiiiii...iiiiiiiiiii..iiiiiiiiiii...iiiiiiii....iiiiiiiii...iiiiiiiiii..iiiiiiiiii...iiiiiiiii....iiiiiiiiii...iiiiiiiii..iiiiiiiii...iiiiiiiiii....iiiiiiiiiii...iiiiiiii..iiiiiiii...iiiiiiiiiii....iiiiiiiiiiii...iiiiiii..iiiiiii...iiiiiiiiiiii....iiiiiiiiiiiii...iiiiii..iiiiii...iiiiiiiiiiiii....iiiiiiiiiiiiii...iiiii..iiiii...iiiiiiiiiiiiii....iiiiiiiiiiiiiii...iiii..iiii...iiiiiiiiiiiiiii....iiiiiiiiiiiiiiii...iii..iii...iiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiii...ii..ii...iiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiii..ii..ii..iiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiii....ii..........................................ii....ii..........................................ii....iiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiii..ii..ii..iiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiii...ii..ii...iiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiii...iii..iii...iiiiiiiiiiiiiiii....iiiiiiiiiiiiiii...iiii..iiii...iiiiiiiiiiiiiii....iiiiiiiiiiiiii...iiiii..iiiii...iiiiiiiiiiiiii....iiiiiiiiiiiii...iiiiii..iiiiii...iiiiiiiiiiiii....iiiiiiiiiiii...iiiiiii..iiiiiii...iiiiiiiiiiii....iiiiiiiiiii...iiiiiiii..iiiiiiii...iiiiiiiiiii....iiiiiiiiii...iiiiiiiii..iiiiiiiii...iiiiiiiiii....iiiiiiiii...iiiiiiiiii..iiiiiiiiii...iiiiiiiii....iiiiiiii...iiiiiiiiiii..iiiiiiiiiii...iiiiiiii....iiiiiii...iiiiiiiiiiii..iiiiiiiiiiii...iiiiiii....iiiiii...iiiiiiiiiiiii..iiiiiiiiiiiii...iiiiii....iiiii...iiiiiiiiiiiiii..iiiiiiiiiiiiii...iiiii....iiii...iiiiiiiiiiiiiii..iiiiiiiiiiiiiii...iiii....iii...iiiiiiiiiiiiiiii..iiiiiiiiiiiiiiii...iii....ii...iiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiii...ii....ii..iiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiii..ii....iiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii......................................................................................................" },
      {"tag": "5-maze","arrity":5 ,"map": "......................................................................................................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....ii.................ii...............ii......ii....ii.................ii...............ii......ii....ii..iiiiiiiiiiii..iiiiiiiiiiii..iiiiiiiiii..ii....ii..iiiiiiiiiiii..iiiiiiiiiiii..iiiiiiiiii..ii....ii..ii.....................ii...........ii..ii....ii..ii.....................ii...........ii..ii....ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii....ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiii....ii..ii..ii.......ii.................ii..iiiiii....iiiiii..ii.......ii.................iiiiii..ii....iiiiii..ii..iiiiiiii..iiiiiiiiiiii..iiiiii..ii....ii..ii..ii..iiiiiiii..iiiiiiiiiiii..ii..ii..ii....ii..ii..ii..ii.........ii.......ii..ii..ii..ii....ii..ii..ii..ii.........ii.......ii..ii..ii..ii....ii..ii..ii..ii..iiiiiiiiiiiiii..ii......ii..ii....ii..ii..ii..ii..iiiiiiiiiiiiii..ii......ii..ii....ii..ii..ii..ii......ii......ii..iiiiii..ii..ii....ii..ii..iiiiii......ii......ii..iiiiii..ii..ii....ii..ii..iiiiii..ii..iiiiii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..iiiiii..ii..ii..ii..ii..ii....ii..ii..ii..iiiiii..iiiiii..iiiiii..ii..iiiiii....ii..ii..ii..iiiiii..iiiiii..iiiiii..ii..iiiiii....ii..ii..ii..ii..ii..iiiiii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..iiiiii..ii..ii..ii..ii..ii....ii..ii..ii......ii......ii..ii..ii..ii..ii..ii....ii..ii..ii......ii......ii..ii..ii..ii..ii..ii....ii..ii..iiiiii..iiiiiiiiiiiiii..ii..ii..ii..ii....iiiiii..iiiiii..iiiiiiiiiiiiii..ii..ii..ii..ii....iiiiii..ii..ii..................ii..iiiiii..ii....ii..ii..ii..ii..................ii..iiiiii..ii....ii..ii..ii..iiiiiiiiiiiiiiiiiiiiii..ii..ii..ii....ii......ii..iiiiiiiiiiiiiiiiiiiiii..ii..ii..ii....ii......ii..........ii..............ii..ii..ii....ii..iiiiii..........ii..............ii..ii..ii....ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii....ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii....ii..ii..................................ii..ii....ii..ii..................................ii..ii....ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiii..ii....ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiii..ii....ii...............................ii..ii.....ii....ii...............................ii..ii.....ii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii......................................................................................................" },
      {"tag": "5-grid","arrity":5 ,"map":"......................................................................................................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....i........i........i........i........i........i....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii......................................................................................................" },
      {"tag": "5-open","arrity":5 ,"map":"......................................................................................................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii......................................................................................................" },
      {"tag": "10-maze","arrity":10 ,"map":"..........................................................................................................................................................................................................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....ii.........................ii.................................................................ii....ii.........................ii.................................................................ii....ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii....ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii....ii..ii................................ii..................................................ii..ii....ii..ii................................ii..................................................ii..ii....ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii....ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii....ii..ii..ii................................................ii..........................ii..ii..ii....ii..ii..ii................................................ii..........................ii..ii..ii....ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii....ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii....iiiiii......ii.................ii.................................................ii..ii..ii..ii....iiiiii......ii.................ii.................................................ii..ii..ii..ii....ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii....ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii....ii..ii..ii..ii..ii...............................ii...........................ii..ii..ii..ii..ii....ii..ii..ii..ii..ii...............................ii...........................ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..iiiiii....ii..iiiiii..ii..ii..ii....................................................ii..ii..ii..ii..iiiiii....ii..iiiiii..ii..ii..ii....................................................ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii.................ii...............ii........ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii.................ii...............ii........ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..iiiiii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..iiiiii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii........ii..........................ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..iiiiii..ii........ii..........................ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..iiiiii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..iiiiii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..........ii................ii..ii..iiiiii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii......ii..ii..ii..........ii................ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii......ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii......ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii...............ii...ii..ii..ii..ii..ii..ii......ii..ii..ii....ii..ii..ii..ii..ii..ii..iiiiii..iiiiii...............ii...ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..iiiiii..ii..ii..iiiiii..iiiiii..iiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..iiiiii..ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii.....ii.....ii..ii..ii..iiiiii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii.....ii.....ii..iiiiii..iiiiii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..iiiiiiii..ii..iiiiii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii......iiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii......iiiiiiii..ii..ii..ii..ii..ii..ii..ii..iiiiii..ii..ii....ii..iiiiii..ii..ii..iiiiii..ii..ii..ii..ii..iiiiiiii..ii..ii..ii..ii..ii..ii..ii..iiiiii..ii..ii....ii..iiiiii..ii..ii..iiiiii..ii..ii..ii..ii..iiiiiiii..ii..ii..ii..ii..ii......ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..iiiiiiii..ii..ii..ii..ii..ii......ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..iiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii......ii..ii.....ii.....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii......ii..ii.....ii.....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..ii.............ii.....ii..ii..ii..ii..ii..ii..ii..ii..iiiiii....iiiiii..ii..ii..ii..ii..ii..ii..ii..ii.............ii.....ii..ii..ii..ii..ii..ii..ii..ii..iiiiii....iiiiii..ii..ii..ii..ii..ii..iiiiii..iiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..iiiiii..iiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..........ii................ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..ii..........ii................ii..ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..iiiiii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..iiiiii..ii..ii..ii..ii..ii....ii..ii..iiiiii..ii..ii..ii..ii...........................ii.......ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..iiiiii..ii..ii..ii..ii...........................ii.......ii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii.......................ii...................ii..ii..iiiiii..ii..ii..ii....ii..ii..ii..ii..ii..ii..ii.......................ii...................ii..ii..iiiiii..ii..ii..ii....ii..ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii....ii..ii..ii..iiiiii..ii..iiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii..ii....ii..ii..ii..iiiiii..ii....................................................ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..ii....................................................ii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii..ii....ii..ii..ii..ii..ii.............................ii.............................ii..ii..ii..ii..ii....ii..ii..ii..ii..ii.............................ii.............................ii..ii..ii..ii..ii....ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii....ii..ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii..ii..ii..ii....ii..ii..ii..ii.............ii...........................ii........................ii..ii..ii..ii....ii..ii..ii..ii.............ii...........................ii........................ii..ii..ii..ii....ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiii..ii..ii..ii....ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiii..ii..ii..ii....ii..ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiii..ii..ii..ii....ii..ii..ii............................................................................ii..ii..ii....ii..ii..ii............................................................................ii..ii..ii....ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiii....ii..ii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..iiiiii....ii..ii....................................................................................ii..ii....iiiiii....................................................................................ii..ii....iiiiiiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii....ii..iiiiiiiiiii..iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii..ii....ii...........................iii...............................ii.............................ii....ii...........................iii...............................ii.............................ii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.........................................................................................................................................................................................................." },
      {"tag": "10-blocks", arrity:10, "map": "..........................................................................................................................................................................................................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiii............iiiiiiiiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiii............iiiiiiiiiiiiiii............iiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiii............iiiiiiiiiiiiiii............iiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiii............iiiiiiiiiiiiiii............iiiiiiiii............iiiii............iiiiiiiiiiii....iiiiiii...................iiiiiiii............iiiiiiiii............iiiii............iiiiiiiiiiii....iiiiiii...................iiiiiiii.............iiiiiiii............iiiii............iiiiiiiiiiii....iiiiiii...................iiiiiiii.............iiiiiiii............iiiii............iiiiiiiiiiii....iiiiiii...................iiiiiiii.............iiiiiiii............iiiii............iiiiiiiiiiii....iiiiiii...................iiiiiiii.............iiiiiiii............iiiii............iiiiiiiiiiii....iiiiiii...................iiiiiiii.............iiiiiiii............iiiii............iiiiiiiiiiii....iiiiiii...................iiiiiiii.............iiiiiiii............iiiii............iiiiiiiiiiii....iiiiiii...................iiiiiiii.............iiiiiiii............iiiii............iiiiiiiiiiii....iiiiiiiiiiiiii............iiiiiiii.............iiiiiiii............iiiii............iiiiiiiiiiii....iiiiiiiiiiiiii............iiiiiiii.............iiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiii............iiiiiiii.............iiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiii............iiiiiiii.............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................iiiiiiiiiiiiiiiiiiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiii............iii....iiiiiiiiiiiiii............iiiiiiiiiiiiiii..................................iiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...........................iiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii...............iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii......................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiii............iiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiii............iiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiii............iiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiii............iiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiii............iiiiii..................................iiiiiiiiiiiiiii............iiiiiiiiiiii....iiiii............iiiiii...............................iiiiiiiiiiiiiiiiii............iiiiiiiiiiii....iiiii............iiiiii........................iiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiii....iiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.............iiiiiiiiiii....iiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.............iiiiiiiiiii....iiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.............iiiiiiiiiii....iiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.............iiiiiiiiiii....iiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii............iiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiiiiii.....................iiiiiiiiiiiiiii............iiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiii............iiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiii............iiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiii............iiiiiiiiiiiiiii.............iiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiii............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiii........................iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiii...............iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii....iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.........................................................................................................................................................................................................."}
    ];
    
    $scope.gridMap=$scope.gridMaps[0];
    $scope.arrity = $scope.gridMap.arrity;
    
    $scope.simpleObString=$scope.gridMap.map;
    $scope.simpleObArray =[];
    //for the 50x50 obarray
    for (var i=0;i<$scope.simpleObString.length;i++) {
		if ($scope.simpleObString[i] !== "i") {
			$scope.simpleObArray.push(i);
			}
		}; 
    
    // global variable
    socket = new WebSocket('ws://192.168.1.10:8080/websocket');
    
    var json = JSON.stringify({"type":"setup","arrity":$scope.arrity,"swarmSize":$scope.swarmSize,"popSize":$scope.popSize,"obArray":$scope.simpleObArray,"items":$scope.foodAmount});
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
	$scope.simpleObArray =[];
	//for the 50x50 obarray
	for (var i=0;i<$scope.gridMap.map.length;i++) {
		if ($scope.gridMap.map[i] !== "i") {
			$scope.simpleObArray.push(i);
			}
		}; 
    // its not very elegant ... but I need to put the images back for the textures
    d3.select("svg").html('<defs><pattern id="grass" x="0" y="0" height="64" width="64" patternUnits="userSpaceOnUse" ><image height="64" width="64" xlink:href="img/grasstile.jpg"></image></pattern><pattern id="brick" x="0" y="0" height="50" width="50" patternUnits="userSpaceOnUse" ><image height="50" width="50" xlink:href="img/bricktile.jpg"></image></pattern></defs>');
    
    var new_json = JSON.stringify({"type":"setup",
      "arrity":$scope.gridMap.arrity,
      "swarmSize":$scope.swarmSize,
      "popSize":$scope.popSize,
      "obArray":$scope.simpleObArray,
      "items":$scope.foodAmount
      });
      
    if (socket.readyState != 0) {
      socket.send(new_json);
    };
    
    socket.onmessage = function(evt) {
      // the onmessage event hook triggers this
      var data = JSON.parse(evt.data);
      // draw the grid
      var gridInfo = data.pop();
      setup_grid(gridInfo.rows,gridInfo.tileSize,$scope.gridScale,gridInfo.obs);
      draw_circles(data,$scope.gridScale,$scope);
      };
    };

      $scope.start = function() {
	//runningflag is used to manage ng-disable on html elements
	$scope.runningFlag=!$scope.runningFlag;
	$scope.controlsFlag =true;
	var startjson = JSON.stringify({"type":"start"});
	  socket.send(startjson);
	  //dont care about the return
	  socket.onmessage = function (x) {;};
	  
	  var dummy_json = JSON.stringify({"type":"report"});
	    //This sends the "update" message to the socket every X ms
	    // and updates the circles with the recived data
	    setInterval(function() {doUpdate()},600);
	      
	    function doUpdate() {
	      socket.send(dummy_json);
	      socket.onmessage = function(evt) {
		var report_json = JSON.parse(evt.data);
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
