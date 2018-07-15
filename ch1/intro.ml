type id = string

type binop = Plus | Minus | Times | Div

type stm
= CompoundStm of stm * stm
| AssignStm of id * exp
| PrintStm of exp list
and exp
= IdExp of id
| NumExp of int
| OpExp of exp * binop * exp
| EseqExp of stm * exp

let prog =
    CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
        CompoundStm(AssignStm("b",
            EseqExp(PrintStm[IdExp "a"; OpExp(IdExp "a", Minus, NumExp 1)],
                OpExp(NumExp 10, Times, IdExp "a"))),
        PrintStm[IdExp "b"]))
(* a = 5 + 3; b = (print(a, a - 1), 10 * a); print(b)  *)

let maxargs st =
    let rec f s =
        match s with
          CompoundStm (s1, s2) -> max (f s1) (f s2)
        | AssignStm (_, e1) -> g e1
        | PrintStm lst ->
            let recResult = List.fold_left max 0 (List.map g lst) in
            max (List.length lst) recResult
    and g e = 
        match e with
          IdExp _-> 0
        | NumExp _ -> 0
        | OpExp (e1, _, e2) -> max (g e1) (g e2)
        | EseqExp (s1, e1) -> max (f s1) (g e1)
    in
    f st ;;

exception ERROR of string

let assertEquals a b = if a <> b then raise (ERROR ((string_of_int a) ^ "<>" ^ (string_of_int b))) ;;
let assertMaxArgs prog n = assertEquals (maxargs prog) n in
assertMaxArgs prog 2;
assertMaxArgs (PrintStm[IdExp("a")]) 1;
assertMaxArgs (PrintStm[IdExp("a"); NumExp(7)]) 2;
assertMaxArgs (PrintStm[EseqExp(PrintStm[IdExp("a"); NumExp(7)], NumExp(2))]) 2;
assertMaxArgs (AssignStm("a", NumExp(42))) 0;
assertMaxArgs (CompoundStm(AssignStm("a", NumExp(42)), PrintStm[IdExp("a"); NumExp(7)])) 2;
assertMaxArgs (PrintStm[OpExp(IdExp("a"), Plus, NumExp(10))]) 1 ;;

type table = (id * int) list 

let interp s =
    let rec lookup t id =
        match t with
          [] -> raise (ERROR ("Symbol " ^ id ^ " not found in table."))
        | (id', value) :: _ when id = id' -> value
        | _ :: tl -> lookup tl id
    in
    let opfunc op =
        match op with
          Plus -> (+)
        | Minus -> (-)
        | Times -> ( * )
        | Div -> (/)
    in
    let rec interpStm s tbl =
        match s with
          CompoundStm (s1, s2) -> let tbl' = interpStm s1 tbl in interpStm s2 tbl'
        | AssignStm (id1, exp1) -> let res, tbl' = interpExp exp1 tbl in (id1, res) :: tbl'
        | PrintStm lst ->
            let foldFun currTbl exp =
                let res, nextTbl = interpExp exp currTbl in
                print_endline (string_of_int res); nextTbl
            in
            List.fold_left foldFun tbl lst
    and interpExp e tbl =
        match e with
          IdExp id1 -> (lookup tbl id1), tbl
        | NumExp num -> num, tbl
        | OpExp (e1, op, e2) -> 
            let res1, tbl' = interpExp e1 tbl in
            let res2, tbl'' = interpExp e2 tbl' in
            (opfunc op) res1 res2, tbl''
        | EseqExp (s1, e1) -> 
            let tbl' = interpStm s1 tbl in
            interpExp e1 tbl'
    in
    let _ = interpStm s [] in
    ()