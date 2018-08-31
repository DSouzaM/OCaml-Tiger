module A = Absyn
module S = Symbol

type depth = int
type escEnv = (depth * bool ref) S.table

let rec traverseVar ((env, d, s): escEnv * depth * Absyn.var): unit =
  match s with
    A.SimpleVar (sym, _) -> (match S.look (env, sym) with
      Some (d', escapes) -> if d > d' then escapes := true
    | None -> ErrorMsg.impossible ("Variable entry not found for " ^ (S.name sym) ^ " while calculating escapes"))
  | A.FieldVar (var, _, _) -> traverseVar (env, d, var)
  | A.SubscriptVar (var, _, _) -> traverseVar (env, d, var)
and traverseExp ((env, d, s): escEnv * depth * Absyn.exp): unit =
  let traverseExps exps =
    let _ = List.map (fun exp -> traverseExp (env, d, exp)) exps in
    ()
  in
  match s with
    A.VarExp var -> traverseVar (env, d, var)
  | A.CallExp {args; _} -> traverseExps args
  | A.OpExp {left; right; _} -> traverseExps [left; right]
  | A.RecordExp {fields; _} ->
    let fieldValues = List.map (fun (_, fieldValue, _) -> fieldValue) fields in
    traverseExps fieldValues
  | A.SeqExp lst ->
    let exps = List.map (fun (exp, _) -> exp) lst in
    traverseExps exps
  | A.AssignExp {var; exp; _} -> let _ = traverseVar (env, d, var) in traverseExp (env, d, exp)
  | A.IfExp {test; then'; else'; _} ->
    let toTraverse = match else' with
      Some exp -> [test; then'; exp]
    | None -> [test; then']
    in
    traverseExps toTraverse
  | A.WhileExp {test; body; _} -> traverseExps [test; body]
  | A.ForExp {var; escape; lo; hi; body; _} ->
    (* look for escapes in lo and hi, then enter var into environment and traverse body with new var *)
    let _ = traverseExps [lo; hi] in
    let env' = S.enter (env, var, (d, escape)) in
    traverseExp (env', d, body)
  | A.LetExp {decs; body} ->
    let env' = traverseDecs (env, d, decs) in
    traverseExp (env', d, body)
  | A.ArrayExp {size; init; _} -> traverseExps [size; init]
  | _ -> ()
and traverseDecs ((env, d, s): escEnv * depth * Absyn.dec list): escEnv =
  let traverseFundec currentEnv ({params; body; _}: Absyn.fundec) =
    (* enter params into table, then recurse on body with new table *)
    let newDepth = d+1 in
    let addParams currentEnv ({name; escape; _}: Absyn.field) = S.enter (currentEnv, name, (newDepth, escape)) in
    let env' = List.fold_left addParams currentEnv params in
    traverseExp (env', newDepth, body)
  in
  let traverseDec currentEnv = function
    (* add variable to environment *)
    A.VarDec {name; escape; _} -> S.enter (currentEnv, name, (d, escape))
  | A.FunctionDec lst ->
    (* process escapes within each function declaration *)
    let _ = List.map (traverseFundec currentEnv) lst in
    (* the variable environment doesn't change outside of these function decs *)
    currentEnv
  | _ -> currentEnv
  in
  List.fold_left traverseDec env s

let findEscape(prog: Absyn.exp) =
	let table = S.empty in
  traverseExp (table, 0, prog)