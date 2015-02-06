(function(){
  var app =angular.module('swarm',[]);
  
  app.controller('SwarmController', function(){
  
  });
  
  app.controller('PanelController', function(){
    this.panelData = saneDefaultGrid;
    this.arrity="wtf";
    this.update = function(newPanelData) {
      this.panelData = newPanelData;
        console.log(this.panelData);
    };
    
    //this.isSelected = function(checkTab) {
      //return this.tab === checkTab;
    //};
    
  });
  
  
  app.controller('SwarmController', function(){
   // this.review = {};
    
    //this.addReview = function(product) {
     // product.reviews.push(this.review);
     // this.review = {};
    //};
  });
  
  
  var saneDefaultGrid = 
    {
      type:'setup',
      arrity:5,
      tileSize:50,
      gridScale:2,
      swarmSize:50,
      popSize:50
    };


})();
