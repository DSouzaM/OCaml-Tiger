module A = Absyn

let print (outstream, e0) =
  let say s = output_string outstream s in
  let sayln s = (say s; say "\n") in
  let rec indent = function
      0 -> () 
    | i -> (say " "; indent (i-1))
  in
  let opname = function
      A.PlusOp -> "PlusOp"
    | A.MinusOp -> "MinusOp"
    | A.TimesOp -> "TimesOp"
    | A.DivideOp -> "DivideOp"
    | A.EqOp -> "EqOp"
    | A.NeqOp -> "NeqOp"
    | A.LtOp -> "LtOp"
    | A.LeOp -> "LeOp"
    | A.GtOp -> "GtOp"
    | A.GeOp -> "GeOp"
  in
  let rec dolist d f = function
      a :: [] -> (sayln ""; f (a, d+1))
    | a :: r -> (sayln ""; f (a, d+1); say ","; dolist d f r)
    | [] -> ()
  in
  let rec var = function
      (A.SimpleVar (s,p), d) -> (indent d; say "SimpleVar(";
          say (Symbol.name s); say ")")
    | (A.FieldVar (v,s,p), d) -> (indent d; sayln "FieldVar(";
          var (v, d+1); sayln ","; indent (d+1);
          say (Symbol.name s); say ")")
    | (A.SubscriptVar (v,e,p), d) -> (indent d; sayln "SubscriptVar(";
          var (v,d+1); sayln ","; exp (e, d+1); say ")")
  and exp = function
      (A.VarExp v, d) -> (indent d; sayln "VarExp("; var (v, d+1); say ")")
    | (A.NilExp, d) -> (indent d; say "NilExp")
    | (A.IntExp i, d) -> (indent d; say "IntExp("; say (string_of_int i);
          say ")")
    | (A.StringExp (s,p), d) -> (indent d; say "StringExp(\"";
          say s; say "\")")
      | (A.CallExp {func; args; pos}, d) ->
          (indent d; say "CallExp("; say (Symbol.name func);
          say ",["; dolist d exp args; say "])")
    | (A.OpExp {left; oper; right; pos}, d) ->
          (indent d; say "OpExp("; say (opname oper); sayln ",";
          exp (left, d+1); sayln ","; exp (right, d+1); say ")")
    | (A.RecordExp {fields; typ; pos}, d) ->
          let f ((name, e, pos), d) =
            (indent d; say "("; say (Symbol.name name);
            sayln ","; exp (e, d+1); say ")")
          in 
            indent d; say "RecordExp("; say (Symbol.name typ);
            say ",["; dolist d f fields; say "])"
    | (A.SeqExp l, d) ->
          (indent d; say "SeqExp["; dolist d exp (List.map (fun (a,_) -> a) l);
          say "]")
    | (A.AssignExp {var=v; exp=e; pos}, d) ->
          (indent d; sayln "AssignExp("; var (v, d+1); sayln ",";
          exp (e, d+1); say ")")
    | (A.IfExp {test; then'; else'; pos}, d) ->
          (indent d; sayln "IfExp("; exp (test, d+1); sayln ",";
          exp (then', d+1);
          (match else' with
            None -> ()
          | Some e -> (sayln ",");
          exp (e, d+1)); say ")")
    | (A.WhileExp {test; body; pos}, d) ->
          (indent d; sayln "WhileExp("; exp (test, d+1); sayln ",";
          exp (body, d+1); say ")")
    | (A.ForExp {var=v; escape=b; lo; hi; body; pos}, d) ->
          (indent d; sayln "ForExp("; indent (d+1); say (Symbol.name v); say ",";
          say (string_of_bool (!b)); sayln ","; exp (lo, d+1); sayln ",";
          exp (hi, d+1); sayln ","; exp (body, d+1); say ")")
    | (A.BreakExp p, d) ->
          (indent d; say "BreakExp")
    | (A.LetExp {decs; body; pos}, d) ->
          (indent d; say "LetExp(["; dolist d dec decs; sayln "],";
          exp (body, d+1); say")")
    | (A.ArrayExp {typ; size; init; pos}, d) ->
          (indent d; say "ArrayExp("; say (Symbol.name typ); sayln ",";
          exp (size, d+1); sayln ","; exp (init, d+1); say ")")
  and dec = function
    | (A.FunctionDec l, d) ->
          let field ({name; escape; typ; pos}, d : Absyn.field * int) =
            (indent d; say "("; say (Symbol.name name);
            say ","; say (string_of_bool (!escape));
            say ","; say (Symbol.name typ); say ")")
          in
          let f ({name; params; result; body; pos}, d : Absyn.fundec * int) =
            (indent d; say "("; say (Symbol.name name); say ",[";
            dolist d field params; sayln "],"; indent d;
            (match result with
              None -> say "NONE"
            | Some (s, p) -> (say "SOME("; say (Symbol.name s); say ")")
            );
            sayln ","; exp(body,d+1); say ")")
          in
            indent d; say "FunctionDec["; dolist d f l; say "]"
    | (A.VarDec {name; escape; typ; init; pos}, d) ->
          (indent d; say "VarDec("; say (Symbol.name name); say ",";
          say (string_of_bool (!escape)); say ",";
          (match typ with
            None -> say "NONE"
          | Some (s, p) -> (say "SOME("; say (Symbol.name s); say ")")
          );
          sayln ","; exp (init, d+1); say ")")
    | (A.TypeDec l, d) ->
          let tdec ({name; ty=t; pos}, d : Absyn.typedec * int) = 
              (indent d; say"("; say (Symbol.name name); sayln ",";
              ty (t, d+1); say ")")
          in
            indent d; say "TypeDec["; dolist d tdec l; say "]"
  and ty = function
      (A.NameTy (s,p), d) ->
          (indent d; say "NameTy("; say (Symbol.name s); say ")")
    | (A.RecordTy l, d) ->
          let f ({name; escape; typ; pos}, d : Absyn.field * int) =
              (indent d; say "("; say (Symbol.name name);
              say ","; say (string_of_bool (!escape)); say ",";
              say (Symbol.name typ); say ")")
          in
            indent d; say "RecordTy["; dolist d f l; say "]"
    | (A.ArrayTy (s, p), d) ->
          (indent d; say "ArrayTy("; say (Symbol.name s); say ")")
  in
    exp (e0, 0); sayln ""; flush outstream


