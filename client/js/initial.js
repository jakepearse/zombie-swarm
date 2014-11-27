function start() {
  var dummy_json = JSON.stringify({"type":"report"});
  //This sends the "update" message to the socket every 1000ms
  // and updates the circles with the recived data
  setInterval(function() {doUpdate()},700);
  function doUpdate() {
    socket.send(dummy_json);
    socket.onmessage = function(evt) {
    var json = JSON.parse(evt.data);
    update_circles(json);
  };
};
};

function setup_grid(arrity,tileSize,gridScale) {
    var list =[];
  for (var i = 0; i <= arrity; i++) {
    list.push(i*tileSize);
  };
  var svg = d3.select("svg")
    .attr("height",(arrity*tileSize)*gridScale)
    .attr("width",(arrity*tileSize)*gridScale)
    .selectAll("line")
    .data(list)
    .enter().append("line")
    .attr("class","xline")
    .attr("x1",0)
    .attr("x2",(arrity*tileSize)*gridScale)
    .attr("y1",function(d) { return d*gridScale; })
    .attr("y2",function(d) { return d*gridScale; })
    .attr("stroke","lightblue");
  };


  



  //}
  //console.log(list);

  //var Scale= d3.scale.linear()
    //.domain(list)
    //.range(list);
  
  //var xAxis = d3.svg.axis()
    //.scale(Scale);
  
  //var Yaxis =d3.svg.axis()
    //.scale(arrity);
  
  //var svg= d3.select("svg");
    //svg.append("g")
    //.attr("class", "x axis")
    //.call(xAxis);
  //};

function updateArrity(arrity) {
    
    
};


function dragmove(d) {
    d3.select(this)
      .style("top", ((d3.event.sourceEvent.pageY) - this.offsetHeight/2)+"px")
      .style("left", ((d3.event.sourceEvent.pageX) - this.offsetWidth/2)+"px")
}

var drag = d3.behavior.drag()
    .on("drag", dragmove);

  function draw_circles(data) {
var svg = d3.select("svg");
    svg.selectAll("circle")
    .data(data)
    .enter().append("circle")
    .on("click", function() {changeColour(this);})
    .style("fill", "steelblue")
    .attr("r", 5)
    .attr("cx", function(d) { return d[1]*5; })
    .attr("cy", function(d) { return d[2]*5; });
}
function changeColour(object) {
    d3.select(object)
    .style("fill","red");
  }
  
