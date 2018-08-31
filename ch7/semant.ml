module A = Absyn
module S = Symbol
module T = Types
module Tr = Translate

type venv = Env.enventry S.table
type tenv = T.ty S.table
type env = {venv: venv; tenv: tenv}

type expty = {exp: Tr.exp; ty: T.ty}

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
let rec transExp (venv, tenv, level, break, exp) =
  let rec trexp = function
    A.VarExp var -> transVar (venv, tenv, level, break, var)

  | A.NilExp -> et (Tr.nilExp ()) T.NIL

  | A.IntExp num -> et (Tr.intExp num) T.INT

  | A.StringExp (str, _) -> et (Tr.stringExp str) T.STRING

  | A.CallExp {func; args; pos} ->
    let funcName = quote (S.name func) in
    (* ensure func is a function *)
    let decLevel, label, formals, result = match getValue venv func pos with
      Env.FunEntry {level; label; formals; result} -> level, label, formals, result
    | _ -> 
      let _ = error pos (funcName ^ " is not a function.") in
      raise SemanticError
    in
    (* ensure the arguments are of the correct type and number *)
    let rec checkParams params arguments translated = match params, arguments with
      [], [] -> translated
    | _, [] -> error pos ("Not enough arguments supplied to " ^ funcName ^ "."); []
    | [], _ -> error pos ("Too many arguments supplied to " ^ funcName ^ "."); []
    | paramHd::paramTl, argHd::argTl ->
      let {ty=argTy; exp=argExp} = trexp argHd in
      let actualParamHd = actualTy paramHd pos in
      if not (checkTy actualParamHd argTy) then
        let i = (List.length translated) + 1 in
        let _ = error pos ("Type of argument " ^ (string_of_int i)
          ^ " to function call " ^ funcName ^ " should be "
          ^ (tyStr actualParamHd) ^ " (" ^ (tyStr argTy) ^ " found).")
        in
        []
      else checkParams paramTl argTl (translated @ [argExp])
    in
    let translatedArgs = checkParams formals args [] in
    let _ = checkErr () in
    let hasReturnValue = result <> T.NIL in
    let callExp = Tr.callExp (label, decLevel, level, translatedArgs, hasReturnValue) in
    et callExp (actualTy result pos)

  | A.OpExp {left; oper; right; pos} ->
    let op = quote (opStr oper) in
    let isStringOp, isRecArrOp = match oper with
      A.EqOp | A.NeqOp -> true, true
    | A.LtOp | A.LeOp | A.GtOp | A.GeOp -> true, false
    | _ -> false, false
    in
    let isIntOnlyOp = not isStringOp in
    let {ty=lTy; exp=lExp} as lExpty = trexp left in
    let {ty=rTy; exp=rExp} as rExpty = trexp right in 
    (* type check operands *)
    let _ =
      let lTyStr = tyStr lTy in
      let rTyStr = tyStr rTy in
      if isIntOnlyOp then
        match (checkInt (lExpty, pos)), (checkInt (rExpty, pos)) with
          false, false -> error pos ("Both operands to " ^ op ^ " are not integers.")
        | false, true -> error pos ("Left operand to " ^ op ^ " is not an integer.")
        | true, false -> error pos ("Right operand to " ^ op ^ " is not an integer.")
        | _ -> ()
      else if (not (checkTy lTy rTy)) && (not (checkTy rTy lTy)) then
        error pos ("Types of operands to " ^ op ^ " do not match. The left operand is "
        ^ lTyStr ^ "; the right is " ^ rTyStr ^ ".")
      else (* types are the same, verify that ops are valid for types *)
        match lTy with
          T.INT -> ()
        | T.STRING -> if not isStringOp then
            error pos ("Cannot apply " ^ op ^ " to string operands.")
        | _ -> if not isRecArrOp then
            error pos ("Cannot apply " ^ op ^ " to record/string operands.")
    in
    let _ = checkErr () in
    let opExp = Tr.opExp (oper, lExp, rExp) in
    et opExp T.INT

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
    let rec checkFields formals actuals translated = match formals, actuals with
      [], [] -> translated
    | (sym, _)::_, [] -> error pos ("Missing field " ^ (quote (S.name sym))
        ^ " in " ^ typeName ^ " instantiation."); []
    | [], (sym,_,_)::_ -> error pos ("Unknown field " ^ (quote (S.name sym))
        ^ " in " ^ typeName ^ " instantiation."); []
    | (fSym, fTy)::fTl, (aSym, exp, _)::aTl ->
      let actualFTy = actualTy fTy pos in
      let fName = quote (S.name fSym) in
      let aName = quote (S.name aSym) in  
      if fSym <> aSym then
        let _ = error pos ("Expected field " ^ fName ^ ", but found "
         ^ aName ^ " in " ^ typeName ^ " instantiation.")
        in
        [] 
      else
        let {ty=aTy; exp=aExp} = trexp exp in
        if not (checkTy actualFTy aTy) then
          let fTyStr = tyStr actualFTy in
          let aTyStr = tyStr aTy in
          let _ = error pos ("Value of " ^ fName ^ " should be " ^ fTyStr
            ^ ", but found " ^ aTyStr ^ " in " ^ typeName ^ " instantiation.")
         in
         []
        else
          checkFields fTl aTl (translated @ [aExp])
    in
    let translatedFields = checkFields formalFields fields [] in
    let _ = checkErr () in
    let recordExp = Tr.recordExp translatedFields in
    et recordExp ty

  | A.SeqExp lst ->
    let fst = fun (x,_) -> x in
    let exps = List.map fst lst in
    (* typecheck each expression, and yield the last one's result *)
    let combine (translatedExps, _) next =
      let {exp; ty} = trexp next in
      (translatedExps @ [exp], ty)
    in
    let (translatedExps, ty) = List.fold_left combine ([], T.UNIT) exps in
    et (Tr.seqExp translatedExps) ty

  | A.AssignExp {var; exp; pos} ->
    let {ty=varTy; exp=varExp} = transVar (venv, tenv, level, break, var) in
    let {ty=expTy; exp=expExp} = trexp exp in
    (* prevent reassignment of constants, check assignment type *)
    let varTyStr = tyStr varTy in
    let expTyStr = tyStr expTy in
    let _ = 
      if isConst venv var pos then
        error pos ("Cannot reassign a constant variable.")
      else if not (checkTy varTy expTy) then
        error pos ("Cannot assign a value of type " ^ expTyStr ^ " to an lvalue of type " ^ varTyStr ^ ".")
    in
    let _ = checkErr () in
    let assignExp = Tr.assignExp (varExp, expExp) in
    et assignExp T.UNIT

  | A.IfExp {test; then'; else'; pos} ->
    let {ty=testTy; exp=testExp} as testExpTy = trexp test in
    let {ty=thenTy; exp=thenExp} = trexp then' in
    let testTyStr = tyStr testTy in
    let thenTyStr = tyStr thenTy in
    (* type check condition *)
    let _ = if not (checkInt (testExpTy, pos)) then
      error pos ("Condition in if-expression should have type int, not " ^ testTyStr)
    in
    let elseExpOpt = match else' with
      Some exp ->
        let {ty=elseTy; exp=elseExp} = trexp exp in
        let elseTyStr = tyStr elseTy in
        if (not (checkTy thenTy elseTy)) && (not (checkTy elseTy thenTy)) then
          let _ = error pos ("Types of then- and else-branches differ. "
          ^ "Then-branch has type " ^ thenTyStr ^ "; "
          ^ "else-branch has type " ^ elseTyStr ^ ".") in
          None
        else Some elseExp
    | None ->
        if thenTy <> T.UNIT then
          error pos ("The body of an if-expression with no else-branch should be unit, not " ^
          thenTyStr); None
    in
    let _ = checkErr () in
    let ifExp = Tr.ifExp (testExp, thenExp, elseExpOpt) in
    et ifExp thenTy

  | A.WhileExp {test; body; pos} ->
    let {ty=testTy; exp=testExp} as testExpTy = trexp test in
    (* increment loop depth for body *)
    let _ = loop_depth:= !loop_depth + 1 in
    let doneLabel = Temp.newlabel () in
    let {ty=bodyTy; exp=bodyExp} = transExp (venv, tenv, level, doneLabel, body) in
    let _ = loop_depth:= !loop_depth - 1 in
    (* typecheck condition and body *)
    let testTyStr = tyStr testTy in
    let bodyTyStr = tyStr bodyTy in
    let _ = if not (checkInt (testExpTy, pos)) then
      error pos ("Condition in loop should have type int, not " ^ testTyStr)
    in
    let _ = if bodyTy <> T.UNIT then
      error pos ("Body of loop should have a unit return type, not " ^ bodyTyStr)
    in
    let _ = checkErr () in
    let whileExp = Tr.whileExp (testExp, bodyExp, doneLabel) in
    et whileExp T.UNIT

  | A.ForExp {var; escape; lo; hi; body; pos} ->
    (* typecheck bounds, but then rewrite as A.WhileExp *)
    let {ty=loTy; _} as loExpTy = trexp lo in
    let {ty=hiTy; _} as hiExpTy = trexp hi in
    let loTyStr = tyStr loTy in
    let hiTyStr = tyStr hiTy in
    let _ = if not (checkInt (loExpTy, pos)) then
      error pos ("For loop lower bound should have type int, not " ^ loTyStr ^ ".")
    in
    let _ = if not (checkInt (hiExpTy, pos)) then
      error pos ("For loop upper bound should have type int, not " ^ hiTyStr ^ ".")
    in
    (* rewrite here
      for i := lo to hi
        do body

      becomes

      let var i := lo         (a)
          var limit := hi
      in
        if i <= limit then    (b)
          while 1 do
            body;
            if i == limit then break (c)
    *)
    let limit = S.symbol "limit" in
    (* initialize variables with lo and hi (a) *)
    let decs = [
        A.VarDec {name=var; escape=escape; typ=None; init=lo; pos=pos};
        A.VarDec {name=limit; escape=ref false; typ=None; init=hi; pos=pos}
    ] in
    (* guard while loop by first checking that lo <= hi (b) *)
    let guard = A.OpExp {
      left=A.VarExp (A.SimpleVar (var, pos));
      oper=A.LeOp;
      right=A.VarExp (A.SimpleVar (limit, pos));
      pos=pos
    } in
    (* break out of the loop when the next value is beyond the limit (c) *)
    let breakExp = A.IfExp {
      test=A.OpExp {
        left=A.VarExp (A.SimpleVar (var, pos));
        oper=A.EqOp;
        right=A.VarExp (A.SimpleVar (limit, pos));
        pos=pos
      };
      then'=A.BreakExp pos;
      else'=None;
      pos=pos
    } in
    (* loop condition is always true; up to the body to break when necessary*)
    let whileLoop = A.WhileExp {
      test=A.IntExp 1;
      body=A.SeqExp [
        (body, pos);
        (breakExp, pos)
      ];
      pos=pos
    } in
    let rewritten = A.LetExp {
      decs=decs;
      body=A.IfExp {
        test=guard;
        then'=whileLoop;
        else'=None;
        pos=pos
      };
      pos=pos
    } in
    trexp rewritten

  | A.BreakExp pos ->
    if (!loop_depth) <= 0 then 
      let _ = error pos "Break statement used outside of a loop." in
      raise SemanticError
    else
      let breakExp = Tr.breakExp break in
      et breakExp T.UNIT

  | A.LetExp {decs; body; pos} ->
    let combine ({venv; tenv}, exps) dec = 
      let (env, newExps) = transDec (venv, tenv, level, break, dec) in
      (env, exps @ newExps)
    in
    let ({venv=venv'; tenv=tenv'}, decExps) = List.fold_left combine ({venv=venv; tenv=tenv}, []) decs in
    let _ = checkErr () in
    let {ty=bodyTy; exp=bodyExp} = transExp (venv', tenv', level, break, body) in
    let _ = checkErr () in
    let resultExp = Tr.seqExp (decExps @ [bodyExp]) in
    et resultExp bodyTy

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
    let {ty=sizeTy; exp=sizeExp} as sizeExpTy = trexp size in
    let {ty=initTy; exp=initExp} = trexp init in
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
    let arrayExp = Tr.arrayExp (sizeExp, initExp) in
    et arrayExp ty
  in
  trexp exp


and transVar (venv, tenv, level, break, var) = match var with
  A.SimpleVar (sym, pos) ->
  let varStr = S.name sym in
  let ty, access = match getValue venv sym pos with
    Env.VarEntry {ty; access; _} -> actualTy ty pos, access
  | _ ->
    let _ = error pos ("Function " ^ varStr ^ " is being used as a variable.") in
    raise SemanticError
  in
  let simpleVarExp = Tr.simpleVar (access, level) in 
  et simpleVarExp ty

| A.FieldVar (recVar, sym, pos) ->
  let fieldStr = quote (S.name sym) in
  (* ensure that the lvalue is a record type *)
  let {ty=recTy; exp=recExp} = transVar (venv, tenv, level, break, recVar) in
  let fieldList = match recTy with
    T.RECORD (lst, _) -> lst
  | _ ->
    let _ = error pos ("Cannot access field " ^ fieldStr ^ " on a non-record type.") in
    raise SemanticError
  in
  let rec findField i = function
    [] -> 
      let _ = error pos ("Unknown field " ^ fieldStr ^ ".") in
      raise SemanticError
  | (fieldSym, fieldTy)::fieldTl ->
      if fieldSym = sym then
        (i, actualTy fieldTy pos)
      else
        findField (i+1) fieldTl
  in
  let offset, fieldTy = findField 0 fieldList in
  let fieldVarExp = Tr.fieldVar (recExp, offset) in
  et fieldVarExp fieldTy

| A.SubscriptVar (arrVar, exp, pos) ->
  let {ty=arrTy; exp=arrExp} = transVar (venv, tenv, level, break, arrVar) in
  let resultTy = match arrTy with
    T.ARRAY (ty, _) -> actualTy ty pos
  | _ ->
    let _ = error pos ("Cannot take subscript of a non-array type.") in
    raise SemanticError
  in
  let {ty=expTy; exp=expExp} as expExpTy = transExp (venv, tenv, level, break, exp) in
  let expTyStr = tyStr expTy in
  let _ = if not (checkInt (expExpTy, pos)) then
    error pos ("Subscript of array should have type int, not " ^ expTyStr ^ ".")
  in
  let _ = checkErr () in
  let subscriptVarExp = Tr.subscriptVar (arrExp, expExp) in
  et subscriptVarExp resultTy

and transDec (venv, tenv, level, break, dec) = match dec with
  A.VarDec {name; escape; typ; init; pos} ->
  let {ty=initTy; exp=initExp} = transExp (venv, tenv, level, break, init) in
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
  let access = Tr.allocLocal level (!escape) in
  let initializingExp = Tr.varDec (access, initExp) in
  let _ = checkErr () in
  ({venv=S.enter (venv, name, Env.VarEntry {access=access; ty=initTy; const=false}); tenv=tenv}, [initializingExp])

| A.FunctionDec lst ->
  (* create new value environment *)
  let getFieldType ({typ; pos; _} : A.field) = getActualType tenv typ pos in
  let enterFunctionNames (seen, venv) ({name; params; result; pos} : A.fundec) =
    if contains seen name then
      let nameStr = quote (S.name name) in
      let _ = error pos ("Function " ^ nameStr ^ " was defined multiple times in a sequence of adjacent function declarations.") in
      raise SemanticError
    else 
      let formalTys = List.map getFieldType params in
      let formalEscapes = List.map (fun ({escape; _}: A.field) -> !escape) params in
      let resultTy = match result with
        Some (sym, pos) -> getActualType tenv sym pos
      | None -> T.UNIT
      in
      let newLevel = Tr.newLevel {parent=level; name=name; formals=formalEscapes} in
      (name :: seen), S.enter (venv, name, (Env.FunEntry {level=newLevel; label=name; formals=formalTys; result=resultTy}))
  in
  let (_, venv') = List.fold_left enterFunctionNames ([], venv) lst in
  (* then, typecheck each individual function *)
  let typecheckFunc ({name; params; body; pos; _} : A.fundec) =
    let funcName = quote (S.name name) in
    let resultTy, funcLevel = match getValue venv' name pos with
      Env.FunEntry {result; level; _} -> result, level
    | _ -> ErrorMsg.impossible (funcName ^ " is not a function entry while typechecking function bodies.")
    in
    let formalAccesses = Tr.formals funcLevel in
    let enterParams (venv, accesses) ({name; escape; typ; pos} : A.field) =
      let access, rest = match accesses with
        hd::tl -> hd, tl
      | [] -> ErrorMsg.impossible "number of parameter accesses in function level different from number of parameters"
      in
      let paramTy = getActualType tenv typ pos in
      let venv' = S.enter (venv, name, (Env.VarEntry {access=access; ty=paramTy; const=true})) in
      (venv', rest)
    in
    (* create value environment inside function *)
    let venv'', _ = List.fold_left enterParams (venv', formalAccesses) params in
    let {ty=bodyTy; exp=bodyExp} = transExp (venv'', tenv, funcLevel, break, body) in
    if not (checkTy resultTy bodyTy) then
      let resultTyStr = tyStr resultTy in
      let bodyTyStr = tyStr bodyTy in
      let _ = error pos ("Function " ^ funcName ^ " should have result type "
        ^ resultTyStr ^ ", but its body has type " ^ bodyTyStr ^ ".") in
      raise SemanticError
    else
      Tr.procEntryExit {level=funcLevel; body=bodyExp}
  in
  let _ = List.map typecheckFunc lst in
  ({venv=venv'; tenv=tenv}, [])

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
  ({venv=venv; tenv=tenv'}, [])

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
  let _ = Tr.reset () in
  let dummylabel = Temp.newlabel () in
  let {exp; _} = transExp (Env.base_venv, Env.base_tenv, Tr.outermost, dummylabel, ast) in
  let _ = Tr.print exp in
  let frags = Tr.getResult () in
  let _ = print_endline "Frags: " in
  let _ = List.iter Tr.printFrag frags in
  frags