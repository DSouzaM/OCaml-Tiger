let parse filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  ErrorMsg.fileName := filename;
  begin
    try
      Prabsyn.print (stdout, Parser.program Lexer.tokenize lexbuf)
    with Parsing.Parse_error ->
      ErrorMsg.error (-1) "Failed to parse file."
  end;
  if !ErrorMsg.anyErrors then exit 1 else ()

let main () = parse (Sys.argv.(1))

let () = main ()