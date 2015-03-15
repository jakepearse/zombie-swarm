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
  };
  draw_background(arrity,tileSize,gridScale,obArray);
  draw_hlines(arrity,tileSize,gridScale,list);
  draw_vlines(arrity,tileSize,gridScale,list);
}

function draw_background(arrity,tileSize,gridScale,obArray){
  //console.log(obArray);
   var svg = d3.select("svg")
   .selectAll("rect")
   .data(obArray)
   .enter().append("rect")
   .attr("class","ob")
   .attr("height",gridScale*5)
   .attr("width",gridScale*5)
   //.attr("y",function(d) { var y=tileSize; return (Math.floor(d[2]/y)*gridScale*5); })
   //.attr("x",function(d) { var x=tileSize; return ((d[1]%x)*gridScale*5); })
    .attr("y",function(d) { return d[2]*gridScale*5; })
    .attr("x",function(d) { return d[1]*gridScale*5; })
   .style("fill","black")
   
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
  }else if (ob.type==="food"){
    return "img/food.png";
  };
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
  .attr("xlink:href",inv);
  $scope.inspectList.push(d);
  
}

function update_circles(data,gridScale,swarmSize,$scope) {
    
    var svg = d3.select("svg");
    var circles = svg.selectAll("image").data(data, function(d) {return d.id});
    
    // change the xy of the selection
    circles.transition()
    .attr("x", function(d) { return d.x*gridScale; })
    .attr("y", function(d) { return d.y*gridScale; });
    //.duration(300);
    
    // add any new elements in .enter
    circles.enter().append("image")
        .on("click", function(d) {changeColour(this,d,$scope);})
        .attr("class", function(d) {return d.type; })
                .attr("id", function(d) { return d.id; })
                .attr("x",function(d) { return d.x*gridScale; })
        .attr("y", function(d) { return d.y*gridScale; })
        .attr("width",gridScale)
        .attr("height",gridScale)
        .attr("xlink:href", function(d) {return setColour(d);});
      
      // remove any leftover elements
      circles.exit().remove();
};


function draw_circles(data,gridScale,$scope) {
  var svg = d3.select("svg");
      svg.selectAll("image")
        .data(data)
        .enter().append("image")
        .on("click", function(d) {changeColour(this,d,$scope);})
        .attr("class", function(d) {return d.type; })
        .attr("id", function(d) { return d.id; })
        //.style("stroke", function(d) { return strokeColour(d);})
        .attr("x",function(d) { return d.x*gridScale; })
        .attr("y", function(d) { return d.y*gridScale; })
        .attr("width",gridScale)
        .attr("height",gridScale)
        .attr("xlink:href", function(d) {return setColour(d);})
        ;
	};

function update_web(data,gridScale,$scope) {

    var weblines = d3.select("svg").selectAll(".webline"); 
 
    weblines = d3.select("svg").selectAll(".webline").data(data, function(d) {return d.id});

    weblines.transition()
    .attr("x1",function(d) {return (d.x*gridScale)+gridScale/2;})
    .attr("x2",function(d,i) {return (d.z_list[i].x*gridScale)+gridScale/2;})
    .attr("y1",function(d) {return d.y*gridScale;})
    .attr("y2",function(d,i) { return d.z_list[i].y*gridScale; });
    
    weblines.enter().append("line")
    .attr("class","webline")
    .attr("x1",function(d) {return d.x*gridScale+gridScale/2})
    .attr("x2",function(d,i) { return d.z_list[i].x*gridScale+gridScale/2})
    .attr("y1",function(d) {return d.y*gridScale})
    .attr("y2",function(d,i) { return d.z_list[i].y*gridScale; })
    .attr("stroke","red")
    .style("stroke-dasharray","10 5");
          weblines.exit().remove();
    
};
