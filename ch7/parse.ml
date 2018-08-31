let parse filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  ErrorMsg.fileName := filename;
  begin
    try
      let ast = Parser.program Lexer.tokenize lexbuf in
      if !ErrorMsg.anyErrors then exit 1 else ast
    with Parsing.Parse_error ->
      ErrorMsg.error (-1) "Failed to parse file.";
      exit 1
  end;
