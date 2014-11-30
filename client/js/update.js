function update_circles(data,gridScale,swarmSize) {
  var anim_time = Math.abs(0.3*swarmSize.value);
  var svg = d3.select("svg");
    svg.selectAll("circle")
    .data(data)
    .transition().attr("cx", function(d) { return d.x*gridScale; })
    .attr("cy", function(d) { return d.y*gridScale; })
    .duration(anim_time);
}


