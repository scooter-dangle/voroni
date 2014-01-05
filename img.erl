-module(img).
-import(lists, [reverse/1]).
-compile(export_all).

parse(Stream) ->
    parse(Stream, []).

parse( <<>> , List) -> reverse(List);
parse(Stream, List) ->
    {Action, Stream2} = delegate(Stream),
    {Token,  Stream3} = Action(Stream2),
    parse(Stream3, [Token|List]).


delegate(<<0:4, Tail/bitstring>>) -> {fun ?MODULE:build_terminate/1, Tail};
delegate(<<0:3, Tail/bitstring>>) -> {fun ?MODULE:build_long_skip/1, Tail};
delegate(<<0:2, Tail/bitstring>>) -> {fun ?MODULE:build_tone/1,      Tail};
delegate(<<0:1, Tail/bitstring>>) -> {fun ?MODULE:build_skip/1,      Tail}.


build_terminate(_) -> {{terminate, 0}, <<>>}.


build_long_skip(Stream) -> build_long_skip(Stream, 0, 0).
build_long_skip(Stream= <<0:1,  _/bitstring>>, Size, _) ->
    {{skip, Size}, Stream};
build_long_skip(<<1:1, Num:8, Tail/bitstring>>, Size, Addend) ->
    build_long_skip(Tail, Size + trunc(Num * math:pow(2, Addend)), Addend + 8);
build_long_skip(<<>>, Size, _) ->
    {{skip, Size}, <<>>}.


build_tone(Stream) -> build_tone(Stream, 0, 0).
build_tone(Stream= <<0:1,  _/bitstring>>, Tone, _) ->
    {{tone, Tone}, Stream};
build_tone(<<1:1, Num:8, Tail/bitstring>>, Tone, Addend) ->
    build_tone(Tail, Tone + trunc(Num * math:pow(2, Addend)), Addend + 8);
build_tone(<<>>, Tone, _) ->
    {{tone, Tone}, <<>>}.



build_skip(<<1:1, Tail/bitstring>>) -> {{skip, 1}, Tail}.



dump(Tokens) -> dump(reverse(Tokens), <<>>).

dump([],                     Stream) -> Stream;
dump([{Action, Count}|Tail], Stream) ->
    Bitstring = apply(?MODULE, Action, [Count]),
    dump(Tail, <<Bitstring/bitstring, Stream/bitstring>>).


terminate(0) -> <<0:4>>.


skip(1)     -> <<1:2>>;
skip(Count) ->
    Int = util:integer_to_bin(Count),
    <<1:4, Int/bitstring>>.


tone(Count) ->
    Int = util:integer_to_bin(Count),
    <<1:3, Int/bitstring>>.

