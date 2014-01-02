-module(img_main).
-compile(export_all).


main_json("-f",     File) -> main_json("--file", File);
main_json("--file", File) ->
  util:img_list_to_json(
    parse_file(File));

main_json("-b",          String) -> main_json("--bitstring", String);
main_json("--bitstring", String) ->
  util:img_list_to_json(
    bitstring(String)).

help() ->
  io:format("~n"),
  io:format("\tUsage:\timg.escript [json] option [file|bitstring]~n"),
  io:format("~n"),
  io:format("\tOptions:~n"),
  lists:foreach(fun ({{switches, Switches},
                      {argument, Argument},
                      {description, Description}}) ->
                    io:format("\t\t~s\s~s\t~s~n", [util:join(", ", Switches), Argument, Description])
                end, options()),
  io:format("~n").

options() ->
  [
   {{switches,
     ["json"]},
    {argument,
     "<opt> <target>"},
    {description,
     "\tUse with either --file or --bitstring to generate json"}},

   {{switches,
     ["-h", "--help"]},
    {argument,
     "\t"},
    {description,
     "\tDisplay this message"}},

   {{switches,
     ["-f", "--file"]},
    {argument,
     "<file>"},
    {description,
     "\tParse binary image at <file>"}},

   {{switches,
     ["-b", "--bitstring"]},
    {argument,
     "<bitstring>"},
    {description,
     "Parse image directly from fake bitstring (ascii zeroes & ones)"}},

   {{switches,
     ["--binary"]},
    {argument,
     "<bitstring>"},
    {description,
     "\tGenerates binary from fake bitstring (ascii zeroes & ones)"}}
  ].

parse_file(File) ->
  case file:read_file(File) of
    {error, enoent} ->
      io:format(standard_error, "File not found:\t~s~n", [File]),
      "";
    {ok, Bitstring} ->
      img:parse(Bitstring)
  end.

binary(String) ->
  erlang:binary_to_list(
    util:bitstring_to_binary(
      util:binstring_to_bitstring(String))).

bitstring(String) ->
  img:parse(
    util:bitstring_to_binary(
      util:binstring_to_bitstring(String))).

string_out("")     -> ok;
string_out(String) -> io:format("~s~n", [String]).

erlang_out("")     -> ok;
erlang_out(String) -> io:format("~p~n", [String]).

