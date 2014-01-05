puts 'loading requirements'
require 'chunky_png'
require 'rake'
require 'json'
#require 'pry'

class Integer
    def to_rgba
        return [0, 0, 0, 0] if zero?

        to_s(16).rjust(8, ?0)
            .chars.each_slice(2)
            .map(&:join)
            .map {|x| x.to_i 16 }
    end
end


puts 'loading png'
png = nil
png = ChunkyPNG::Image.from_file FileList['*.png'].first

width  = png.dimension.width
height = png.dimension.height

puts 'constructing output array for #to_rgba'
out = nil
out = Array.new(width) { Array.new(height) { [0,0,0,0] } }

def out.atat x, y
    at(x).at(y)
end

def out.color x, y
    (x_int, y_int) = [x.round, y.round]
    (x1_fact, x2_fact,
     y1_fact, y2_fact) = [1 - (x - x_int + 0.5), x - x_int + 0.5,
                          1 - (y - y_int + 0.5), y - y_int + 0.5]

    (a1_fact, b1_fact,
     a2_fact, b2_fact) = [x1_fact * y1_fact, x2_fact * y1_fact,
                          x1_fact * y2_fact, x2_fact * y2_fact]

    colors = [atat(x_int.pred, y_int.pred), atat(x_int, y_int.pred),
              atat(x_int.pred, y_int),      atat(x_int, y_int)]

    out = []

    ###############################################################
    #                                                             #
    #              o = (x, y) is the point in question.           #
    #                                                             #
    #              x_int.pred       (x_int - 0.5)          x_int  #
    #                 v                   v                  v    #
    #   y_int.pred > a1................x..|..................b1   #
    #                 .                |                     .    #
    #                 .                |                     .    #
    #                 .   b2_fact      |      a2_fact        .    #
    #                 .             y2_fact                  .    #
    #                 .                |                     .    #
    #                 .                |                     .    #
    #                 y---x2_fact------o------x1_fact--------.    #
    #                 .                |                     .    #
    # (y_int - 0.5) > -                |                     .    #
    #                 .             y1_fact                  .    #
    #                 .                |                     .    #
    #                 .   b2_fact      |      a1_fact        .    #
    #                 .                |                     .    #
    #                 .                |                     .    #
    #                 .                |                     .    #
    #                 .                |                     .    #
    #        y_int > a2......................................b2   #
    #                                                             #
    ###############################################################
    colors.shift.zip(*colors) do |a1, b1, a2, b2|
        out << ((a1 * a1_fact) + (b1 * b1_fact) +
                (a2 * a2_fact) + (b2 * b2_fact)).round
    end

    out
end

def out.pad!
    each {|column| column.push column.last, column.first }
    push last, first
end

out.pad!


#threads = []

start = Time.now
puts 'transferring pixels to target array with #to_rgba'
thread_chunk_size = 800
(0...width).each_slice(thread_chunk_size) do |chunk|
    # Threading is only worth it with much larger images (in which case,
    # it's still only worth it if you're using jruby or possibly rubinius)
    #puts "Starting thread to manage #{chunk.first} through #{chunk.last}"
    #threads << Thread.new do
        chunk.each do |x|
            height.times {|y|
                out.atat(x, y).replace png[x, y].to_rgba
            }
        end
        #puts "Finished #{chunk.first} through #{chunk.last}"
    #end
end

#threads.each &:join
#threads.clear

out.pad!

puts "Time elapsed during transfer:\t#{Time.now - start}"

coörds = JSON.parse(IO.read 'co-ords.json').map {|(x, y)|
    [(x / 100) * width, (y / 100) * height]
}.map {|(x, y)|
    [:tone, out.color(x, y)]
}

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


coörd_string = "module.exports = #{JSON.dump coörds}"
IO.write 'co-ords_img.js', coörd_string

