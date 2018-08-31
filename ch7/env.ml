module T = Types
module S = Symbol

type access = unit
type ty = T.ty
type enventry = VarEntry of {access: Translate.access; ty: ty; const: bool}
              | FunEntry of {level: Translate.level; label: Temp.label; formals: ty list; result: ty}

let enter_to_table tbl lst =
  let enter_single tbl (name, el) = S.enter (tbl, S.symbol name, el) in
  List.fold_left enter_single tbl lst

let base_tenv : ty S.table =
  let base_types = [
    ("int", T.INT);
    ("string", T.STRING)
  ] in
  enter_to_table S.empty base_types

let base_venv : enventry S.table =
  let base_funcs = [
    ("print", [T.STRING], T.UNIT);
    ("flush", [], T.UNIT);
    ("getchar", [], T.STRING);
    ("ord", [T.STRING], T.INT);
    ("chr", [T.INT], T.STRING);
    ("size", [T.STRING], T.INT);
    ("substring", [T.STRING; T.INT; T.INT], T.STRING);
    ("concat", [T.STRING; T.STRING], T.STRING);
    ("not", [T.INT], T.INT);
    ("exit", [T.INT], T.UNIT)
  ]
  in
  let makeFunEntry (name, params, result) =
    let newLabel = Temp.newlabel () in
    let newFormals = List.map (fun x -> false) params in
    let newLevel = Translate.newLevel {parent=Translate.outermost; name=newLabel; formals=newFormals} in
    (name, FunEntry {level=newLevel; label=newLabel; formals=params; result=result})
  in
  enter_to_table S.empty (List.map makeFunEntry base_funcs)