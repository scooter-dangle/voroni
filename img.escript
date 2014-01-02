#!/usr/bin/env escript
%%! -smp enable -sname img

%-mode(compile).

main(["-f",     File]) -> main(["--file", File]);
main(["--file", File]) -> img_main:erlang_out(img_main:parse_file(File));

main(["--binary", String]) -> img_main:string_out(img_main:binary(String));

main(["-b",          String]) -> main(["--bitstring", String]);
main(["--bitstring", String]) -> img_main:erlang_out(img_main:bitstring(String));

main(["json", Method, String]) -> img_main:string_out(img_main:main_json(Method, String));

main([])         -> main(["--help"]);
main(["-h"])     -> main(["--help"]);
main(["--help"]) -> img_main:help();

main(_) ->
  io:format(standard_error, "Unknown option:\tRun with --help for usage~n", []).

