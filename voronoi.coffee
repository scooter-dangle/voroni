# Credit: The intial outline of this code was from the canonical
# d3 Voronoi example

d3 = require 'd3'
screen = require './screen'
lister = require './co-ords_img.js'

w = 800
h = 600

to_rgba = ([r, g, b, a]) -> "rgba(#{r},#{g},#{b},#{a / 255})"

vertices = screen.read(lister).map ({x, y, color}) ->
  out = [(x / 100) * w, (y / 100) * h]
  out.color = to_rgba color
  out

# Transforms an array of polygon vertices into the d attribute
# string used by an svg path element.
mz = (d) -> "M#{d.join 'L'}Z"

wrap = (wrapperA, wrapperB='') ->
  (d) -> "#{wrapperA}#{d}#{wrapperB}"

identity = (d) -> d

d3.select('body')
  .append('div')
  .attr('id', 'chart')

svg = d3.select("#chart")
  .append("svg:svg")
  .attr("width",  w)
  .attr("height", h)

svg.selectAll("path")
  .data(d3.geom.voronoi vertices)
  .enter().append("svg:path")
  .attr("fill",   ({point: {color}}) -> color)
  .attr("stroke", ({point: {color}}) -> color)
  .attr("d", mz)

svg_document  = d3.select("#chart").html()
html_document = "<!doctype html><html><style>path {stroke-width: 0.75px;}</style><head></head><body><div id='chart'>#{svg_document}</div></body></html>"
console.log html_document
