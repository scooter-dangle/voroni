screen = require './screen'

s = new screen

size = 75000

s.read_one ['tone', 0] for _ in [0..size]

s.read_one ['terminate', 0]

coörds = s.pixels.map ({x, y}) -> [x, y]

console.log JSON.stringify coörds
