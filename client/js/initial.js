function start() {
  inspector.value ="";
  var gridScale = document.getElementById('gridScale').value;
  var dummy_json = JSON.stringify({"type":"report"});
  //This sends the "update" message to the socket every 1000ms
  // and updates the circles with the recived data
  setInterval(function() {doUpdate()},1000);
  function doUpdate() {
    socket.send(dummy_json);
    socket.onmessage = function(evt) {
    var json = JSON.parse(evt.data);
    update_circles(json,gridScale);
    for (var i = 0; i<inspectList.length; i++) {
      var element = inspectList[i];
      inspector.value += "id: "+element.__data__[0]+", X: "+element.__data__[1]+"\n";
    }
    };
  };
};


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
}


function draw_hlines(arrity,tileSize,gridScale,list) {
   var svg = d3.select("svg")
    .attr("height",(arrity*tileSize)*gridScale)
    .attr("width",(arrity*tileSize)*gridScale)
    .selectAll("hline")
    .data(list)
    .enter().append("line")
    .attr("class","xline")
    .attr("x1",0)
    .attr("x2",(arrity*tileSize)*gridScale)
    .attr("y1",function(d) { return d*gridScale; })
    .attr("y2",function(d) { return d*gridScale; })
    .attr("stroke","lightblue");
}


function draw_vlines(arrity,tileSize,gridScale,list) {
    var svg = d3.select("svg")
    .selectAll(".vline")
    .data(list)
    .enter().append("line")
    .attr("class","yline")
    .attr("y1",0)
    .attr("y2",(arrity*tileSize)*gridScale)
    .attr("x1",function(d) { return d*gridScale; })
    .attr("x2",function(d) { return d*gridScale; })
    .attr("stroke","lightblue");
};


function updateArrity(arrity) {
   ; //dummy
};


function draw_circles(data,gridScale) {
  var svg = d3.select("svg");
      svg.selectAll("circle")
      .data(data)
      .enter().append("circle")
      .on("click", function() {changeColour(this);})
      .style("fill", "steelblue")
      .attr("r", gridScale)
      .attr("cx", function(d) { return d[1]*gridScale; })
      .attr("cy", function(d) { return d[2]*gridScale; });
}


function changeColour(object) {
  d3.select(object)
  .style("fill","red")
  .attr("class","inspect-me");
  inspectList.push(object);
}
  
