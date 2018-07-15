let parse filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let lexer = Lex.tokenize lexbuf in
  let rec do_it () =
    let t = lexer in
    print_endline lexer;
    if String.sub t 0 3 = "EOF" then () else do_it ()
  in
    do_it ()


let main () = parse (Sys.argv.(1))

let () = main ()