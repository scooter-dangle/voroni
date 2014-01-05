# encoding: utf-8
puts 'loading requirements'
require 'chunky_png'
require 'rake'
require 'json'
require 'pry'
# Not yet used
require 'ruby_vor'

class Numeric
    def opaque?
        self == 255
    end

    def transparent?
        zero?
    end

    def bound min, max
        return max if self > max
        return min if self < min
        self
    end
end

class Integer
    def to_rgba
        # If integer is zero, then alpha channel is zero
        # and the pixel is fully transparent. Assuming a
        # white background, then, the pixel will render
        # as entirely white.
        return [Array::R, Array::B, Array::G, 255] if transparent?

        to_s(16).rjust(8, ?0)
            .chars.each_slice(2)
            .map(&:join)
            .map {|x| x.to_i 16 }
    end

    def to_rgb
        to_rgba.from_rgba_to_rgb
    end
end

class Array
    # white
    BACKGROUND = [255, 255, 255]
    R, B, G = BACKGROUND

    def atat x, y
        at(x).at(y)
    end

    def from_rgba_to_rgb
        r, g, b, a = self
        return Array::BACKGROUND.dup if a.transparent?
        return [r, g, b] if a.opaque?

        conversion_factor = (255 - a) / 255.0

        # Need to double-check this method for non-white
        # backgrounds
        r += ((Array::R - r) * conversion_factor).round.bound 0, 255
        g += ((Array::G - g) * conversion_factor).round.bound 0, 255
        b += ((Array::B - b) * conversion_factor).round.bound 0, 255

        [r, g, b]
    end

    def rgb_distance_from( (r2, g2, b2) )
        r1, g1, b1 = self
        (r1 - r2).abs + (g1 - g2).abs + (b1 - b2).abs
    end

    def euclidean_distance_from ary
        out = 0
        zip(ary) {|a, b| out += (a - b)**2 }
        Math.sqrt out
    end

    def pad!
        push last, first
    end

    # See also the diagram in out.color...having called out.pad! first is
    # necessary for out.color to work. (But out.pad! can only be called
    # after out has finished being filled with the colors from the png
    # image.)
    def matrix_pad!
        each &:pad!
        pad!
    end
end


puts 'loading png'
png = ChunkyPNG::Image.from_file FileList['*.png'].first

width  = png.dimension.width
height = png.dimension.height

puts 'constructing output array for #to_rgba'
# Is the following faster than first initializing
# a blank out array?
pixels = Array.new(width) {|x|
    Array.new(height) {|y|
        png[x, y].to_rgb
    }
}
pixels.matrix_pad!

# out = Array.new(width) { Array.new(height) { [0,0,0,0] } }

def pixels.color x, y
    (x_int, y_int) = [x.round, y.round]
    (x1_fact, x2_fact,
     y1_fact, y2_fact) = [1 - (x - x_int + 0.5), x - x_int + 0.5,
                          1 - (y - y_int + 0.5), y - y_int + 0.5]

    (a1_fact, b1_fact,
     a2_fact, b2_fact) = [x1_fact * y1_fact, x2_fact * y1_fact,
                          x1_fact * y2_fact, x2_fact * y2_fact]

    # There's nothing special about the head color vs the tail colors...
    # We'll be zipping them all together, but we need a target array
    # upon which to call the Array#zip method
    (head_color,
    *tail_colors) = [atat(x_int.pred, y_int.pred), atat(x_int, y_int.pred),
                     atat(x_int.pred, y_int),      atat(x_int, y_int)]

    out = []

    ###############################################################
    #                                                             #
    #              o = (x, y) is the point in question.           #
    #                                                             #
    #              The entire square of the diagram below would   #
    #              have the color of the a1 co-ordinate when in   #
    #              the png image. However, with svg we're working #
    #              with a percentage-based co-ordinate system     #
    #              rather than an array co-ordinate system (i.e., #
    #              we'll have an exact center point of x = 50%    #
    #              and y = 50% in svg, whereas in a png array,    #
    #              we'd only have an exact mid-point if the       #
    #              height or width of the pixel matrix is an odd  #
    #              number). So we'd only use the color located at #
    #              a1 without modification if our point were      #
    #              exactly                                        #
    #                  o = (x_int.pred + 0.5, y_int.pred + 0.5).  #
    #              This point is noted by a '%' mark on the       #
    #              diagram below.                                 #
    #                                                             #
    #              Note: Bounds checking is obviated by using     #
    #              out.pad!, which first appends the bottom       #
    #              border and right border to the bottom and      #
    #              right of the pixel map, so that something      #
    #              like o = (1.75, 1.75) will work for a pixel    #
    #              map with respective height and widths of 2     #
    #              pixels. Next, out.pad! appends the top and     #
    #              left border to the bottom and right sides,     #
    #              respectively, so that when x or y are less     #
    #              than 0.5 (so that x_int.pred or y_int.pred     #
    #              are -1) calling out.atat(-1, _) or             #
    #              out.atat(_, -1) will still access the left or  #
    #              top pixel (respectively). Does that make       #
    #              sense?                                         #
    #                                                             #
    #                                                             #
    #              x_int.pred     (x_int - 0.5)          x_int    #
    #                 v                 v                  v      #
    #   y_int.pred > a1-----------------|--x--------------b1      #
    #                 |                    |              |       #
    #                 |                 .  |              |       #
    #                 |      b2_fact       |    a2_fact   |       #
    #                 |                 .  |              |       #
    #                 |                 y2_fact           |       #
    #                 |                 .  |              |       #
    # (y_int - 0.5) > -   .   .   .   . % .|  .   .   .   |       #
    #                 |                 .  |              |       #
    #                 y------x2_fact-------o----x1_fact---|       #
    #                 |                 .  |              |       #
    #                 |                 y1_fact           |       #
    #                 |      b1_fact    .  |    a1_fact   |       #
    #                 |                    |              |       #
    #                 |                 .  |              |       #
    #        y_int > a2-----------------------------------b2      #
    #                                                             #
    ###############################################################
    head_color.zip(*tail_colors) do |a1, b1, a2, b2|
        # This operation is called many times...it would be good
        # to know if using Array#inject vs just using plus signs
        # adds a performance penalty.
        out << [(a1 * a1_fact), (b1 * b1_fact),
                (a2 * a2_fact), (b2 * b2_fact)].inject(:+).round
    end

    out
end

#threads = []

start = Time.now
puts 'transferring pixels to target array with #to_rgba'
#thread_count = 1
#thread_chunk_size = (width / thread_count.to_f).ceil
#(0...width).each_slice(thread_chunk_size) do |chunk|
    # Threading is only worth it with much larger images (in which case,
    # it's still only worth it if you're using jruby or possibly rubinius)
    #puts "Starting thread to manage #{chunk.first} through #{chunk.last}"
    #threads << Thread.new do
        #chunk.each do |x|
            #height.times {|y|
                #pixels.atat(x, y).replace png[x, y].to_rgba
            #}
        #end
        #puts "Finished #{chunk.first} through #{chunk.last}"
    #end
#end

#threads.each &:join
#threads.clear


puts "Time elapsed during transfer:\t#{Time.now - start}"

coörds = JSON.parse(IO.read 'co-ords.json').map {|(x, y)|
    [(x / 100) * width, (y / 100) * height]
}.map {|(x, y)|
    [:tone, pixels.color(x, y)]
}

coörds.pry
exit

class Array
    def zero_tone?
        self == [:tone, [0, 0, 0, 0]]
    end

    def zero_tone_or_skip?
        zero_tone? or at(0) == :skip
    end
end


# See slow_compress for more idiomatic reading of
# what compress_1 does
compress_1 = ->(list) do
    skip = [:skip, 1]
    initial_chunk = list[0, 4].map(&:last).map(&:last).map(&:zero?)
    l1, l2, candidate, r1, r2 = [false, *initial_chunk]

    for i in (4...(list.length)) do
        l1, l2,        candidate, r1, r2 =
        l2, candidate, r1,        r2, (list.at(i).at(1).at(3) == 0)

        list[i - 2] = skip if
            l1 and l2 and candidate and r1 and r2
    end

    list
end

# A *much* slower but terser version of compress_1 is as follows
slow_compress = ->(list) do
    (list.length - 5).times do |i|
        list[i + 2] = [:skip, 1] if
            list[i, 5].all?(&:zero_tone_or_skip?)
    end

    list
end

puts "trying to reduce the size just a bit"
compress_1[coörds]

# Any transparency turns out poorly due to imprecision in the stroke width
# of the voronoi polygons...if you don't have at least some stroke width,
# you end up with uncovered gaps between polygons, but if you have enough
# stroke width to avoid gaps, you'll have some overlap between the polygons,
# which is fine unless the strokes are opaque. If they're partially transparent,
# they add together to create geometric paths that stand out from the image. :(
puts "converting zero-tones to opaque all-whites and removing all transparency"
coörds.select do |(action, _)|
    action == :tone
end.each do |(_, rgba)|
    rgba.fill 255 if rgba == [0,0,0,0]
end.reject do |(_, (*_, a))|
    # Filter out out any fully opaque cells
    # before next round
    a == 255
end.each do |(_, rgba)|
    # Here we're operating on any fully or partially
    # transparent cells. Assuming a white background,
    # we'll scale their rgb values toward 255 by the
    # degree toward which we need to modify their
    # 'a' (opacity) value to bring it to 255
    r, g, b, a = rgba
    conversion_factor = (255 - a) / 255.0

    r += ((255 - r) * conversion_factor).round
    g += ((255 - g) * conversion_factor).round
    b += ((255 - b) * conversion_factor).round

    r, g, b = [r, 255].min,
              [g, 255].min,
              [b, 255].min

    rgba.replace [r, g, b, 255]
end

coörd_string = "module.exports = #{JSON.dump coörds}"
IO.write 'co-ords_img.js', coörd_string

