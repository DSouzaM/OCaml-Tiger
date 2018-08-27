let run filename =
  try
    let ast = Parse.parse filename in
    let _ = FindEscape.findEscape ast in
    let _ = Semant.transProg ast in
    Prabsyn.print (stdout, ast)
  with Semant.SemanticError -> print_endline ("Failed to typecheck \"" ^ filename ^ "\"."); exit 1

let main () = run (Sys.argv.(1))

let () = main ()