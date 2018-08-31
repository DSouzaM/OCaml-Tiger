module T = Tree

let printtree (outstream, s) =
	let say s = output_string outstream s in
	let sayln s = (say s; say "\n") in
	let rec indent = function
  	  0 -> ()
  	| i -> (say " "; indent (i-1))
  in
  let rec stm (s, d) =
    let _ = indent d in
    match s with
      T.SEQ (a,b) -> (sayln "SEQ("; stm (a, d+1); sayln ","; stm(b, d+1); say ")")
    | T.LABEL lab -> (say "LABEL "; say (Symbol.name lab))
    | T.JUMP (e,_) -> (sayln "JUMP("; exp (e, d+1); say ")")
    | T.CJUMP (r,a,b,t,f) -> (say "CJUMP(";
        relop r; sayln ",";
        exp (a, d+1); sayln ",";
        exp (b, d+1); sayln ",";
        indent (d+1); say (Symbol.name t); say ","; say (Symbol.name f); say ")")
    | T.MOVE (a,b) -> (sayln "MOVE("; exp (a, d+1); sayln ","; exp (b, d+1); say ")")
    | T.EXP e -> (sayln "EXP("; exp (e, d+1); say ")")
  and exp (e, d) =
    let _ = indent d in
    match e with
      T.BINOP (p, a, b) -> (say "BINOP("; binop p; sayln ",";
        exp (a, d+1); sayln ","; exp (b, d+1); say ")")
    | T.MEM e -> (sayln "MEM("; exp (e, d+1); say ")")
    | T.TEMP t -> (say "TEMP "; say (Temp.makestring t))
    | T.ESEQ (s,e) -> (sayln "ESEQ("; stm (s, d+1); sayln ","; exp (e, d+1); say ")")
    | T.NAME lab -> (say "NAME "; say (Symbol.name lab))
    | T.CONST i -> (say "CONST "; say (string_of_int i))
    | T.CALL (e, el) -> (sayln "CALL("; exp (e, d+1);
      match el with
        [] -> ()
      | hd::tl -> (sayln ""; exp (hd, d+1); List.iter (fun arg -> sayln ","; exp (arg, d+1)) tl; say ")"))
  and binop op =
    let str = function
      T.PLUS -> "PLUS"
    | T.MINUS -> "MINUS"
    | T.MUL -> "MUL"
    | T.DIV -> "DIV"
    | T.AND -> "AND"
    | T.OR -> "OR"
    | T.LSHIFT -> "LSHIFT"
    | T.RSHIFT -> "RSHIFT"
    | T.ARSHIFT -> "ARSHIFT"
    | T.XOR -> "XOR"
    in say (str op)
  and relop op =
    let str = function
      T.EQ -> "EQ"
    | T.NE -> "NE"
    | T.LT -> "LE"
    | T.GT -> "GT"
    | T.LE -> "LE"
    | T.GE -> "GE"
    | T.ULT -> "ULT"
    | T.ULE -> "ULE"
    | T.UGT -> "UGT"
    | T.UGE -> "UGE"
    in say (str op)
  in
  stm (s, 0); sayln ""; flush outstream