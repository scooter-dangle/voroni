#!/usr/bin/env escript
%%! -smp enable -sname img

%-mode(compile).

main(["-f",     File]) -> main(["--file", File]);
main(["--file", File]) -> parse_file(File);

main(["--binary", String]) -> binary(String);

main(["-b",          String]) -> main(["--bitstring", String]);
main(["--bitstring", String]) -> bitstring(String);

main([])         -> main(["--help"]);
main(["-h"])     -> main(["--help"]);
main(["--help"]) -> help();

main(_) ->
  io:format(standard_error, "Unknown option:\tRun with --help for usage\n", []).


help() ->
  io:format("\n"),
  io:format("\tUsage:\timg.escript\s[options] [file|bitstring]\n"),
  io:format("\n"),
  io:format("\tOptions:\n"),
  lists:foreach(fun ({{switches, Switches},
                      {argument, Argument},
                      {description, Description}}) ->
                    io:format("\t\t~s\s~s\t~s\n", [util:join(", ", Switches), Argument, Description])
                end, options()),
  io:format("\n").

options() ->
  [
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
      io:format(standard_error, "File not found:\t~s\n", [File]);
    {ok, Bitstring} ->
      Img = img:parse(Bitstring),
      io:format("~w\n", [Img])
  end.

binary(String) ->
  Bitstring = erlang:binary_to_list(util:bitstring_to_binary(util:binstring_to_bitstring(String))),
  io:format("~s\n", [Bitstring]).

bitstring(String) ->
  Img = img:parse(util:bitstring_to_binary(util:binstring_to_bitstring(String))),
  io:format("~w\n", [Img]).

