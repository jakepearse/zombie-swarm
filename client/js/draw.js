//
//	Lines
//
function setup_grid(arrity,tileSize,gridScale,obArray) {
  arrity = parseInt(arrity);
  tileSize = parseInt(tileSize);
  gridScale = parseInt(gridScale);
     var svg=d3.select("svg")
      .attr("height",arrity*tileSize*gridScale)
      .attr("width",arrity*tileSize*gridScale);
  var list =[];
  for (var i = 0; i <= arrity; i++) {
    list.push(i*tileSize)
    draw_background(arrity,tileSize,gridScale,obArray,i);
  };
  //draw_background(arrity,tileSize,gridScale,obArray);
  draw_hlines(arrity,tileSize,gridScale,list);
  draw_vlines(arrity,tileSize,gridScale,list);
}

function draw_background(arrity,tileSize,gridScale,list){
   var svg = d3.select("svg")
   .selectAll("g")
   .data(list)
   .enter().append("g")
   .attr("class","tile")
   
   
    ;
    }

function obColour(b) {
  if (b==="t") {
    return "#000000";}
    return "#FFFFFF";
  };


function draw_hlines(arrity,tileSize,gridScale,list) {
   var svg = d3.select("svg").selectAll("hline")
    .data(list)
    .enter().append("line")
    .attr("class","xline")
    .attr("x1",0)
    .attr("x2",(arrity*tileSize)*gridScale)
    .attr("y1",function(d) { return d*gridScale; })
    .attr("y2",function(d) { return d*gridScale; })
    .attr("stroke","cadetblue")
    .style("stroke-dasharray","10 5");
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
    .attr("stroke","cadetblue")
    .style("stroke-dasharray","10 5");
};

/////////////
// Circles //
/////////////

function setColour(ob) {
  if (ob.type==="human"){
    return "img/hume.png";
  }
  return "img/zomb.png";
}
  
function strokeColour(ob) {
  if (ob.type==="human"){
    return "blue";
  }
  return "seagreen";
}

function changeColour(object,d,$scope) {
  var inv;
  if (d.type==="human"){
    inv = "img/inv_hume.png";
  }else{ inv = "img/inv_zomb.png";}
  d3.select(object)
  //.style("fill","tomato")
  //.style("stroke","indianred")
  .attr("xlink:href",inv);
  $scope.inspectList.push(d);
  
}

function update_circles(data,gridScale,swarmSize) {
 // console.log(data);
  //var anim_time = Math.abs(0.3*swarmSize.value);
    var svg = d3.select("svg");
    svg.selectAll("image")
    .data(data, function(d) { return d.id; })
    .transition()
    .attr("x", function(d) { return d.x*gridScale; })
    .attr("y", function(d) { return d.y*gridScale; })
    .attr("id", function(d) { return d.id; })
    .duration(300);

   svg.selectAll("image")
      .each(function(d,i) {
        //Find corresponding pid in data list
        for (var j=0;j<data.length;j++) {
          if (data[j].id === d.id){
            break;
          } else {
            if(j == (data.length -1)) {
              d3.select(this).remove()
            }
          }
        }
      })
}

function draw_circles(data,gridScale,$scope) {
  var svg = d3.select("svg");
      svg.selectAll("image")
        .data(data)
        .enter().append("image")
        .on("click", function(d) {changeColour(this,d,$scope);})
        .attr("class", function(d) {return d.type; })
        //.attr("r", gridScale)
         //.attr("height", "16")
        // .attr("width","16")
        // .attr("x", function(d) { return d.x*gridScale; })
       // .attr("y", function(d) { return d.y*gridScale; })
        //.attr("cx", function(d) { return d.x*gridScale; })
        //.attr("cy", function(d) { return d.y*gridScale; })
        .attr("id", function(d) { return d.id; })
        //.style("fill",function(d) { return setColour(d);})
        //.attr("fill","url(#zomb)")
        .style("stroke", function(d) { return strokeColour(d);})
        //.style("stroke-width",0.5*gridScale)
        //.attr("viewBox", "0 0 16 16")
        //.append("image")
        .attr("x",function(d) { return (d.x-gridScale/2)*gridScale; })
        .attr("y", function(d) { return (d.y-gridScale/2)*gridScale; })
        .attr("width",gridScale)
        .attr("height",gridScale)
          .attr("xlink:href", function(d) {return setColour(d);})
        ;
	};
