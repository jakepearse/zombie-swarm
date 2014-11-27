function update_circles(data) {
  var svg = d3.select("svg");
    svg.selectAll("circle")
    .data(data)
    .transition().attr("cx", function(d) { return d[1]*5; })
    .transition().attr("cy", function(d) { return d[2]*5; });
}


