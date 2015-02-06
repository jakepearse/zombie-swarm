(function(){
  var app =angular.module('swarm',[]);
  
  app.controller('SwarmController', function(){
  
  });
  
  app.controller('panelController', function(){
    this.PanelData = saneDefaultGrid;
    
    this.update = function(newPanelData) {
      this.panelData = newPanelData;
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
    };

  
})();
