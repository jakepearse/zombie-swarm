function update_circles(data,gridScale) {
  var svg = d3.select("svg");
    svg.selectAll("circle")
    .data(data)
    .transition().attr("cx", function(d) { return d[1]*gridScale; })
    .transition().attr("cy", function(d) { return d[2]*gridScale; });
}


