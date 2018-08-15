module A = Absyn
module S = Symbol
module T = Types

type venv = Env.enventry S.table
type tenv = T.ty S.table
type env = {venv: venv; tenv: tenv}

type expty = {exp: Translate.exp; ty: T.ty}

exception SemanticError

(* string utils *)
let quote str = "\"" ^ str ^ "\""

let tyStr ty = 
  (* first signifies if function is called for the first time *)
  let rec tyStr_ first ty = match ty with
    T.INT -> "int"
  | T.STRING -> "string"
  | T.RECORD (lst, _) ->
    if first then
      match lst with
        [] -> "{}"
      | hd::tl ->
        let strForField (sym, ty) = (S.name sym) ^ ":" ^ (tyStr_ false ty) in
        let combine agg next = agg ^ "; " ^ next in
        let body = List.fold_left combine (strForField hd) (List.map strForField tl) in
        "{" ^ body ^ "}"
    else "record"
  | T.ARRAY (ty, _) ->
    if first then "array of " ^ (tyStr_ false ty)
    else "array"
  | T.NIL -> "nil"
  | T.UNIT -> "unit"
  | T.NAME (name, tyref) -> 
    let prefix = "name(" ^ (S.name name) ^ ", " in
    (match !tyref with
      Some ty -> prefix ^ "some)"
    | None -> prefix ^ "none)")
    
  in
  tyStr_ true ty

let opStr = function
    A.PlusOp -> "+"
  | A.MinusOp -> "-"
  | A.TimesOp -> "*"
  | A.DivideOp -> "/"
  | A.EqOp -> "="
  | A.NeqOp -> "<>"
  | A.LtOp -> "<"
  | A.LeOp -> "<="
  | A.GtOp -> ">"
  | A.GeOp -> ">="


(* helper functions *)
let error pos msg = ErrorMsg.error pos msg

let checkErr () = if !(ErrorMsg.anyErrors) then raise SemanticError

let et exp ty = {exp=exp; ty=ty}

let rec actualTy ty pos = match ty with
  T.NAME (sym, reference) -> (match !reference with
    Some resolved -> actualTy resolved pos
  | None -> 
    let _ = error pos ("The type \"" ^ (S.name sym) ^ "\" has still not been resolved.") in
    raise SemanticError
  )
| _ -> ty

let checkInt ({ty; _}, pos) = actualTy ty pos = T.INT

let getValue venv sym pos = match S.look (venv, sym) with
  Some value -> value
| None ->
  let _ = error pos ("The value \"" ^ (S.name sym) ^ "\" is undefined.") in
  raise SemanticError

let getType tenv sym pos = match S.look (tenv, sym) with
  Some typ -> typ
| None ->
  let _ = error pos ("The type \"" ^ (S.name sym) ^ "\" is undefined.") in
  raise SemanticError

let getActualType tenv sym pos =
  actualTy (getType tenv sym pos) pos

let checkTy expected actual = match expected with
  T.RECORD _ -> expected == actual || actual = T.NIL
| _ -> expected == actual

let isConst venv var pos = match var with 
  A.SimpleVar (sym, _) -> (match getValue venv sym pos with
    Env.VarEntry {const;_} -> const
  | _ -> false)  
| _ -> false

let rec contains lst el = match lst with
  [] -> false
| hd::tl -> el=hd || contains tl el


let loop_depth = ref 0

(* main typechecking functions *)

(*  venv * tenv * Absyn.exp -> expty  *)
let rec transExp (venv, tenv, exp) =
  let rec trexp = function
    A.VarExp var -> transVar (venv, tenv, var)

  | A.NilExp -> et () T.NIL

  | A.IntExp _ -> et () T.INT

  | A.StringExp (_, _) -> et () T.STRING

  | A.CallExp {func; args; pos} ->
    let funcName = quote (S.name func) in
    (* ensure func is a function *)
    let formals, result = match getValue venv func pos with
      Env.FunEntry {formals; result} -> formals, result
    | _ -> 
      let _ = error pos (funcName ^ " is not a function.") in
      raise SemanticError
    in
    (* ensure the arguments are of the correct type and number *)
    let rec checkParams params arguments i = match params, arguments with
      [], [] -> ()
    | _, [] -> error pos ("Not enough arguments supplied to " ^ funcName ^ ".")
    | [], _ -> error pos ("Too many arguments supplied to " ^ funcName ^ ".")
    | paramHd::paramTl, argHd::argTl ->
      let {ty=argTy; _} = trexp argHd in
      let actualParamHd = actualTy paramHd pos in
      if not (checkTy actualParamHd argTy) then
        error pos ("Type of argument " ^ (string_of_int i)
          ^ " to function call " ^ funcName ^ " should be "
          ^ (tyStr actualParamHd) ^ " (" ^ (tyStr argTy) ^ " found).")
      else checkParams paramTl argTl (i+1)
    in
    let _ = checkParams formals args 1 in
    let _ = checkErr () in
    et () (actualTy result pos)

  | A.OpExp {left; oper; right; pos} ->
    let op = quote (opStr oper) in
    let isStringOp, isRecArrOp = match oper with
      A.EqOp | A.NeqOp -> true, true
    | A.LtOp | A.LeOp | A.GtOp | A.GeOp -> true, false
    | _ -> false, false
    in
    let isIntOnlyOp = not isStringOp in
    let {ty=lty;_} as lexpty = trexp left in
    let {ty=rty;_} as rexpty = trexp right in 
    let lTyStr = tyStr lty in
    let rTyStr = tyStr rty in
    let _ = if isIntOnlyOp then
      match (checkInt (lexpty, pos)), (checkInt (rexpty, pos)) with
        false, false -> error pos ("Both operands to " ^ op ^ " are not integers.")
      | false, true -> error pos ("Left operand to " ^ op ^ " is not an integer.")
      | true, false -> error pos ("Right operand to " ^ op ^ " is not an integer.")
      | _ -> ()
    else if (not (checkTy lty rty)) && (not (checkTy rty lty)) then
      error pos ("Types of operands to " ^ op ^ " do not match. The left operand is "
      ^ lTyStr ^ "; the right is " ^ rTyStr ^ ".")
    else (* types are the same, verify that ops are valid for types *)
      match lty with
        T.INT -> ()
      | T.STRING -> if not isStringOp then
          error pos ("Cannot apply " ^ op ^ " to string operands.")
      | _ -> if not isRecArrOp then
          error pos ("Cannot apply " ^ op ^ " to record/string operands.")
    in
    let _ = checkErr () in
    et () T.INT

  | A.RecordExp {fields; typ; pos} ->
    let typeName = S.name typ in
    (* ensure typ is a record type *)
    let ty, formalFields = match getActualType tenv typ pos with
      T.RECORD (lst, _) as ty -> ty, lst
    | _ ->
      let _ = error pos (typeName ^ " is not a record type.") in
      raise SemanticError
    in
    (* ensure each field is instantiated with the right type, in the right order *)
    let rec checkFields formals actuals = match formals, actuals with
      [], [] -> ()
    | (sym, _)::_, [] -> error pos ("Missing field " ^ (quote (S.name sym))
        ^ " in " ^ typeName ^ " instantiation.")
    | [], (sym,_,_)::_ -> error pos ("Unknown field " ^ (quote (S.name sym))
        ^ " in " ^ typeName ^ " instantiation.")
    | (fsym, fty)::ftl, (asym, exp, _)::atl ->
      let actualFty = actualTy fty pos in
      let fname = quote (S.name fsym) in
      let aname = quote (S.name asym) in  
      if fsym <> asym then
        error pos ("Expected field " ^ fname ^ ", but found "
         ^ aname ^ " in " ^ typeName ^ " instantiation.")
      else
        let {ty=aty; _} = trexp exp in
        if not (checkTy actualFty aty) then
          let ftystr = tyStr actualFty in
          let atystr = tyStr aty in
          error pos ("Value of " ^ fname ^ " should be " ^ ftystr
          ^ ", but found " ^ atystr ^ " in " ^ typeName ^ " instantiation.")
        else
          checkFields ftl atl
    in
    let _ = checkFields formalFields fields in
    let _ = checkErr () in
    et () ty

  | A.SeqExp lst ->
    let fst = fun (x,_) -> x in
    let exps = List.map fst lst in
    (* typecheck each expression, and yield the last one's result *)
    let combine _ next = trexp next in
    List.fold_left combine (et () T.UNIT) exps

  | A.AssignExp {var; exp; pos} ->

    let {ty=varTy; _} = transVar (venv, tenv, var) in
    let {ty=expTy; _} = trexp exp in
    let varTyStr = tyStr varTy in
    let expTyStr = tyStr expTy in
    if isConst venv var pos then
      let _ = error pos ("Cannot reassign a constant variable.") in
      raise SemanticError
    else if not (checkTy varTy expTy) then
      let _ =
        error pos ("Cannot assign a value of type " ^ expTyStr ^ " to an lvalue of type " ^ varTyStr ^ ".")
      in
      raise SemanticError
    else
      et () T.UNIT

  | A.IfExp {test; then'; else'; pos} ->
    let {ty=testTy; _} as testExpTy = trexp test in
    let {ty=thenTy; _} = trexp then' in
    let testTyStr = tyStr testTy in
    let thenTyStr = tyStr thenTy in
    let _ = if not (checkInt (testExpTy, pos)) then
      error pos ("Condition in if-expression should have type int, not " ^ testTyStr)
    in
    let _ = match else' with
      Some exp ->
        let {ty=elseTy; _} = trexp exp in
        let elseTyStr = tyStr elseTy in
        if (not (checkTy thenTy elseTy)) && (not (checkTy elseTy thenTy)) then
          error pos ("Types of then- and else-branches differ. "
          ^ "Then-branch has type " ^ thenTyStr ^ "; "
          ^ "else-branch has type " ^ elseTyStr ^ ".")
    | None ->
        if thenTy <> T.UNIT then
          error pos ("The body of an if-expression with no else-branch should be unit, not " ^
          thenTyStr)
    in
    let _ = checkErr () in
    et () thenTy

  | A.WhileExp {test; body; pos} ->
    let {ty=testTy; _} as testExpTy = trexp test in
    let _ = loop_depth:= !loop_depth + 1 in
    let {ty=bodyTy; _} = trexp body in
    let _ = loop_depth:= !loop_depth - 1 in
    let testTyStr = tyStr testTy in
    let bodyTyStr = tyStr bodyTy in
    let _ = if not (checkInt (testExpTy, pos)) then
      error pos ("Condition in while loop should have type int, not " ^ testTyStr)
    in
    let _ = if bodyTy <> T.UNIT then
      error pos ("Body of while loop should have a unit return type, not " ^ bodyTyStr)
    in
    let _ = checkErr () in
    et () T.UNIT

  | A.ForExp {var; lo; hi; body; pos; _} ->
    let {ty=loTy; _} as loExpTy = trexp lo in
    let {ty=hiTy; _} as hiExpTy = trexp hi in
    let loTyStr = tyStr loTy in
    let hiTyStr = tyStr hiTy in
    let venv' = S.enter (venv, var, Env.VarEntry {ty=T.INT; const=true}) in
    let _ = if not (checkInt (loExpTy, pos)) then
      error pos ("For loop lower bound should have type int, not " ^ loTyStr ^ ".")
    in
    let _ = if not (checkInt (hiExpTy, pos)) then
      error pos ("For loop upper bound should have type int, not " ^ hiTyStr ^ ".")
    in
    let _ = loop_depth:= !loop_depth + 1 in
    let {ty=bodyTy; _} = transExp (venv', tenv, body) in
    let _ = loop_depth:= !loop_depth - 1 in
    let bodyTyStr = tyStr bodyTy in
    let _ = if bodyTy <> T.UNIT then
      error pos ("Body of for loop should have a unit return type, not " ^ bodyTyStr ^ ".")
    in
    let _ = checkErr () in
    et () T.UNIT

  | A.BreakExp pos -> if (!loop_depth) <= 0 then 
      let _ = error pos "Break statement used outside of a loop." in
      raise SemanticError
    else et () T.UNIT

  | A.LetExp {decs; body; pos} ->
    let combine {venv; tenv} dec = transDec (venv, tenv, dec) in
    let {venv=venv'; tenv=tenv'} = List.fold_left combine {venv=venv; tenv=tenv} decs in
    let _ = checkErr () in
    let {ty=bodyTy; _} = transExp (venv', tenv', body) in
    let _ = checkErr () in
    et () bodyTy

  | A.ArrayExp {typ; size; init; pos} ->
    let typeName = S.name typ in
    (* ensure typ is an array type *)
    let ty, arrTy = match getActualType tenv typ pos with
      T.ARRAY (arrTy, _) as ty -> ty, actualTy arrTy pos
    | _ ->
      let _ = error pos (typeName ^ " is not an array type.") in
      raise SemanticError
    in
    let arrTyStr = tyStr arrTy in
    let {ty=sizeTy; _} as sizeExpTy = trexp size in
    let {ty=initTy; _} = trexp init in
    let sizeTyStr = tyStr sizeTy in
    let initTyStr = tyStr initTy in
    (* ensure size has type int *)
    let _ = if not (checkInt (sizeExpTy, pos)) then
      error pos ("Size of array should have type int, not " ^ sizeTyStr ^ ".")
    in
    (* ensure initializing value has the correct type *)
    let _ = if not (checkTy arrTy initTy) then
      error pos ("Initializing value for array should have type " ^ arrTyStr
      ^ ", not " ^ initTyStr ^ ".")
    in
    let _ = checkErr () in
    et () ty
  in
  trexp exp

(*  venv * tenv * Absyn.var -> expty  *)
and transVar (venv, tenv, var) = match var with
  A.SimpleVar (sym, pos) ->
  let varStr = S.name sym in
  let ty = match getValue venv sym pos with
    Env.VarEntry {ty; _} -> actualTy ty pos
  | _ ->
    let _ = error pos ("Function " ^ varStr ^ " is being used as a variable.") in
    raise SemanticError
  in
  et () ty

| A.FieldVar (recVar, sym, pos) ->
  let fieldStr = quote (S.name sym) in
  (* ensure that the lvalue is a record type *)
  let {ty=recTy; _} = transVar (venv, tenv, recVar) in
  let fieldList = match recTy with
    T.RECORD (lst, _) -> lst
  | _ ->
    let _ = error pos ("Cannot access field " ^ fieldStr ^ " on a non-record type.") in
    raise SemanticError
  in
  let rec findField field = function
    [] -> 
      let _ = error pos ("Unknown field " ^ fieldStr ^ ".") in
      raise SemanticError
  | (fieldSym, fieldTy)::fieldTl ->
      if fieldSym = field then
        et () (actualTy fieldTy pos)
      else
        findField field fieldTl
  in
  findField sym fieldList

| A.SubscriptVar (arrVar, exp, pos) ->
  let {ty=arrTy; _} = transVar (venv, tenv, arrVar) in
  let resultTy = match arrTy with
    T.ARRAY (ty, _) -> actualTy ty pos
  | _ ->
    let _ = error pos ("Cannot take subscript of a non-array type.") in
    raise SemanticError
  in
  let {ty=expTy; _} as expExpTy = transExp (venv, tenv, exp) in
  let expTyStr = tyStr expTy in
  let _ = if not (checkInt (expExpTy, pos)) then
    error pos ("Subscript of array should have type int, not " ^ expTyStr ^ ".")
  in
  let _ = checkErr () in
  et () resultTy

(*  venv * tenv * Absyn.dec -> {venv: venv; tenv: tenv}  *)
and transDec (venv, tenv, dec) = match dec with
  A.VarDec {name; typ; init; pos; _} ->
  let {ty=initTy; _} = transExp (venv, tenv, init) in
  let initTyStr = tyStr initTy in
  let _ = match typ with
    Some (sym, pos) ->
      let declTy = getActualType tenv sym pos in
      let declTyStr = tyStr declTy in
      if not (checkTy declTy initTy) then
        error pos ("Declared type for " ^ (quote (S.name name))
          ^ " (" ^ declTyStr ^ ") differs from the initializing type ("
          ^ initTyStr ^ ").")
  | None -> if initTy = T.NIL then
      error pos "Cannot initialize variable to nil without an explicit type declaration."
  in
  let _ = checkErr () in
  {venv=S.enter (venv, name, Env.VarEntry {ty=initTy; const=false}); tenv=tenv}

| A.FunctionDec lst ->
  (* create new value environment *)
  let getFieldType ({typ; pos; _} : A.field) = getActualType tenv typ pos in
  let enterFunctionNames (seen, venv) ({name; params; result; pos} : A.fundec) =
    if contains seen name then
      let nameStr = quote (S.name name) in
      let _ = error pos ("Function " ^ nameStr ^ " was defined multiple times in a sequence of adjacent function declarations.") in
      raise SemanticError
    else 
      let formals = List.map getFieldType params in
      let resultTy = match result with
        Some (sym, pos) -> getActualType tenv sym pos
      | None -> T.UNIT
      in
      (name :: seen), S.enter (venv, name, (Env.FunEntry {formals=formals; result=resultTy}))
  in
  let (_, venv') = List.fold_left enterFunctionNames ([], venv) lst in
  (* then, typecheck each individual function *)
  let typecheckFunc ({name; params; body; pos; _} : A.fundec) =
    let funcName = quote (S.name name) in
    let resultTy = match getValue venv' name pos with
      Env.FunEntry {result; _} -> result
    | _ -> ErrorMsg.impossible (funcName ^ " is not a function entry while typechecking function bodies.")
    in
    let enterParams venv ({name; typ; _} : A.field) =
      let paramTy = getActualType tenv typ pos in
      S.enter (venv, name, (Env.VarEntry {ty=paramTy; const=true}))
    in
    (* create value environment inside function *)
    let venv'' = List.fold_left enterParams venv' params in
    let {ty=bodyTy; _} = transExp (venv'', tenv, body) in
    if not (checkTy resultTy bodyTy) then
      let resultTyStr = tyStr resultTy in
      let bodyTyStr = tyStr bodyTy in
      let _ = error pos ("Function " ^ funcName ^ " should have result type "
        ^ resultTyStr ^ ", but its body has type " ^ bodyTyStr ^ ".") in
      raise SemanticError
  in
  let _ = List.map typecheckFunc lst in
  {venv=venv'; tenv=tenv}

| A.TypeDec lst ->
  (* create new type environment with name types *)
  let enterTypeNames (seen, tenv) ({name; pos; _} : A.typedec) =
    if contains seen name then
      let nameStr = S.name name in
      let _ = error pos ("Type " ^ nameStr ^ " was defined multiple times in a sequence of adjacent type declarations.") in
      raise SemanticError
    else
      (name :: seen), S.enter (tenv, name, (T.NAME (name, ref None)))
  in
  let (_, tenv') = List.fold_left enterTypeNames ([], tenv) lst in
  let resolveTypes ({name; ty; pos} : A.typedec) =
    let nameStr = S.name name in
    let resolved = transTy (tenv', ty) in
    let reference = match getType tenv' name pos with
      T.NAME (_, reference) -> reference
    | _ -> ErrorMsg.impossible (nameStr ^ " is not a NAME entry while typechecking type declarations.")
    in
    reference := Some resolved
  in
  (* resolve each type *)
  let _ = List.map resolveTypes lst in
  (* detect cycles *)
  let detectCycle ({name; pos; _} : A.typedec) =
    let ty = getType tenv' name pos in
    let rec detect seen ty = match ty with
      T.NAME (sym, nextTy) ->
        if contains seen sym then
          let _ = error pos ("Cycle detected in type declarations for type " ^ (S.name sym) ^ ".") in
          raise SemanticError
        else
          let unboxedTy = match !nextTy with
            Some x -> x
          | None -> ErrorMsg.impossible ("Type " ^ (S.name sym) ^ " was never resolved.")
          in
          detect (sym :: seen) unboxedTy
    | _ -> ()
    in
    let _ = detect [] ty in
    checkErr ()
  in
  let _ = List.map detectCycle lst in
  {venv=venv; tenv=tenv'}

(*  tenv * Absyn.ty -> Types.ty  *)
and transTy (tenv, ty) = match ty with
  A.NameTy (sym, pos) -> getType tenv sym pos

| A.RecordTy lst ->
  let combine ({name; typ=fieldTySym; pos; _} : A.field) fields =
    let fieldTy = getType tenv fieldTySym pos in
    (name, fieldTy) :: fields
  in
  let fieldList = List.fold_right combine lst [] in
  T.RECORD (fieldList, ref ())

| A.ArrayTy (sym, pos) ->
  let ty = getType tenv sym pos in
  T.ARRAY (ty, ref ())

let transProg ast =
  let _ = transExp (Env.base_venv, Env.base_tenv, ast) in
  ()