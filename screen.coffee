module.exports = do ->
    class Counter
        constructor: (
            @total_count=0,
            @layer=0,
            @count_until_new_layer=1,
            @count_until_new_row=1,
            @rows=1, @columns=1,
            @row=0, @column=0,
            @x=50.0, @y=50.0,
            @x_dir=1, @y_dir=-1,
            @x_incr=100.0, @y_incr=100.0,
            @x_base=50.0, @y_base=50.0
        ) ->

        add: (count = 1) ->
            out = {@x, @y}
            @increment() for _ in [1..count]
            out

        increment: ->
            @total_count += 1
            return @new_layer() if @count_until_new_layer is 1

            @count_until_new_layer -= 1
            if @count_until_new_row is 1
                @count_until_new_row = @columns
                @row += @y_dir
                @x_dir *= -1
            else
                @count_until_new_row -= 1
                @column += @x_dir

            @x = @x_base + (@column * @x_incr)
            @y = @y_base + (@row    * @y_incr)

        new_layer: ->
            @layer += 1
            @rows = @columns = Math.pow 2, @layer
            @count_until_new_row = @columns
            @row = @rows - 1 unless @row is 0
            @y_dir *= -1
            @x_dir = 1
            @count_until_new_layer = @rows * @columns
            @[attr] /= 2 for attr in ['x_incr', 'y_incr', 'x_base', 'y_base']
            @x = @x_base + (@column * @x_incr)
            @y = @y_base + (@row    * @y_incr)


    class Screen
        @read = (token_list=[]) ->
            (new @(token_list)).pixels

        constructor: (token_list=[]) ->
            @pixels = []
            @counter = new Counter

            @read token_list

        read: (token_list) ->
            @read_one token for token in token_list
            @pixels

        read_one: ([action, argument]) ->
            # The following nonsense is necessary in case this is run
            # through something like Google Closure
            switch action
                when 'skip'      then      @skip argument
                when 'tone'      then      @tone argument
                when 'terminate' then @terminate argument
                else 'error'

        skip: (count) ->
            @counter.add count
            null

        tone: (color) ->
            {x, y} = @counter.add 1
            @pixels.push {x, y, color}
            {x, y, color}

        terminate: (_) ->
            @read_one = () -> @pixels
            null
