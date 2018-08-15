let run filename =
  try
    let ast = Parse.parse filename in
    Semant.transProg ast
  with Semant.SemanticError -> print_endline ("Failed to typecheck \"" ^ filename ^ "\"."); exit 1

let main () = run (Sys.argv.(1))

let () = main ()