-module(util).
-compile(export_all).
%%-export([binstring_to_bitstring/1, log/2]).

%% NOTE: Requires json.beam from Yaws

binstring_to_bitstring(String) ->
  binstring_to_bitstring(String, []).

binstring_to_bitstring([],        Bitlist) ->
  list_to_bin(lists:reverse(Bitlist));

binstring_to_bitstring([49|Tail], Bitlist) ->
  binstring_to_bitstring(Tail, [1|Bitlist]);
binstring_to_bitstring([48|Tail], Bitlist) ->
  binstring_to_bitstring(Tail, [0|Bitlist]).

bin_to_list(Stream) -> bin_to_list(Stream, []).

bin_to_list( <<>> , List) -> lists:reverse(List);
bin_to_list(<<X:1, Rest/bitstring>>, List) ->
    bin_to_list(Rest, [X|List]).

list_to_bin(List) -> list_to_bin(lists:reverse(List), <<>>).

list_to_bin([], Stream) -> Stream;
list_to_bin([X|Tail], Stream) ->
    list_to_bin(Tail, <<X:1, Stream/bitstring>>).

log(X, Base) -> math:log(X) / math:log(Base).

integer_to_bin(Int) ->
  list_to_bin(util:integer_to_list(Int)).

integer_to_list(0)   -> [0,0,0,0];
integer_to_list(Int) ->
  insert_separators(
    pad_powers_of_two(
      integer_to_powers_of_two(Int))).

integer_to_powers_of_two(Int) ->
  integer_to_powers_of_two(Int, []).

integer_to_powers_of_two(0,   List) -> List;
integer_to_powers_of_two(Int, List) ->
  Power = erlang:trunc(util:log(Int, 2)),
  Remainder = Int - erlang:trunc(math:pow(2, Power)),
  integer_to_powers_of_two(Remainder, [Power + 1 | List]).

bitstring_to_binary(Bitstring) ->
  Padsize = (8 - (erlang:bit_size(Bitstring) rem 8)) rem 8,
  <<Bitstring/bitstring, 0:Padsize>>.

join(Joiner, Strings) ->
  reduce(fun (A, B) -> A ++ Joiner ++ B end, Strings).

reduce(_, [A])  -> reduce(nothing, [], A);
reduce(Fun, [A, B | Tail]) ->
  reduce(Fun, Tail, Fun(A, B)).

reduce(Fun, [Head | Tail], Out) ->
  reduce(Fun, Tail, Fun(Out, Head));
reduce(_,   [], Out) -> Out.

pad_powers_of_two(List) ->
  Highest = util:highest_product_of_four(lists:last(List)),
  lists:map(
    fun (X) -> bool_to_int(lists:member(X, List)) end,
    lists:seq(1, Highest)
   ).

insert_separators(List) ->
  lists:reverse(insert_separators(List, [])).

insert_separators([A, B, C, D], List) ->
  [A, B, C, D | List];
insert_separators([A, B, C, D | Tail], List) ->
  insert_separators(Tail, [1, A, B, C, D | List]).

bool_to_int(true)  -> 1;
bool_to_int(false) -> 0.

highest_product_of_four(0) -> highest_product_of_four(1);
highest_product_of_four(Int) -> Int + 3 - ((Int + 3) rem 4).

list_to_string(List) ->
  erlang:binary_to_list(
    erlang:list_to_binary(List)
   ).

img_list_to_json(List) ->
  Arrayify = fun ({Token, Count}) -> {array, [erlang:atom_to_list(Token), Count]} end,
  NewList = lists:map(Arrayify, List),
  list_to_string(
    %% NOTE: Requires json.beam from Yaws
    json:encode({array, NewList})
   ).

json_to_img_list(Json) ->
  %% NOTE: Requires json.beam from Yaws
  {ok, {array, List}} = json:decode_string(Json),
  DeArrayify = fun ({array, [Token, Count]}) -> {erlang:list_to_atom(Token), Count} end,
  lists:map(DeArrayify, List).

