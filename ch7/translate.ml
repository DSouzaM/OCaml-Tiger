module T = Tree
module A = Absyn

type exp = Ex of T.exp
         | Nx of T.stm
         | Cx of (Temp.label * Temp.label -> T.stm)

type level = {parent: level option; frame: Frame.frame; eq: unit ref}
type access = level * Frame.access
type frag = Frame.frag
type newLevelParams = {parent: level; name: Temp.label; formals: bool list}
type procEntryExitParams = {level: level; body: exp}

(* fragment code *)
let frags: frag list ref = ref []

let addFrag frag =
  frags := (!frags) @ [frag]


let getResult () = !frags

let reset () =
  frags := []


(* utility functions *)
let levelEq a b =
  let {eq=aeq; _} = a in
  let {eq=beq; _} = b in
  aeq == beq

let rec seq = function
  last::[] -> last
| hd::tl -> T.SEQ(hd, seq tl)
| _ -> ErrorMsg.impossible "seq shouldn't be called on zero statements"

let unEx = function
  Ex exp -> exp
| Nx stm -> T.ESEQ (stm, T.CONST 0)
| Cx genstm ->
  (* convert conditional to 0 (false) or 1 (true) *)
  let r = Temp.newtemp () in
  let t, f = Temp.newlabel (), Temp.newlabel () in
  let steps = [
    T.MOVE (T.TEMP r, T.CONST 1);
    genstm (t, f);
    T.LABEL f;
    T.MOVE (T.TEMP r, T.CONST 0);
    T.LABEL t
  ] in
  T.ESEQ (seq steps, T.TEMP r)

let unNx = function
  Ex exp -> T.EXP exp
| Nx stm -> stm
| Cx genstm ->
  (* regardless of true/false, jump ahead *)
  let lbl = Temp.newlabel () in
  T.SEQ (genstm (lbl, lbl), T.LABEL lbl)

let unCx = function
  Ex T.CONST 0 -> fun (t,f) -> T.JUMP (T.NAME f, [f])
| Ex T.CONST 1 -> fun (t,f) -> T.JUMP (T.NAME t, [t])
| Ex exp -> fun (t,f) -> T.CJUMP (T.NE, exp, T.CONST 0, t, f)
| Nx _ -> ErrorMsg.impossible "a no-result expression should never be coerced to a conditional expression"
| Cx genstm -> genstm

let add x y = T.BINOP (T.PLUS, x, y)
let sub x y = T.BINOP (T.MINUS, x, y)
let mult x y = T.BINOP (T.MUL, x, y)
let div x y = T.BINOP (T.DIV, x, y)

let isRelop = function
  A.EqOp | A.NeqOp | A.LtOp | A.LeOp | A.GtOp | GeOp -> true
| _ -> false

let binopFor = function
  A.PlusOp -> T.PLUS
| A.MinusOp -> T.MINUS
| A.TimesOp -> T.MUL
| A.DivideOp -> T.DIV
| _ -> ErrorMsg.impossible "attempted to find corresponding binary operator in the IR for a non-binary operator in the abstract syntax"

let relopFor = function
  A.EqOp -> T.EQ
| A.NeqOp -> T.NE
| A.LtOp -> T.LT
| A.LeOp -> T.LE
| A.GtOp -> T.GT
| A.GeOp -> T.GE
| _ -> ErrorMsg.impossible "attempted to find corresponding relational operator in the IR for a non-relational operator in the abstract syntax"

(* yield a Tree.exp which represents the address of the frame at decLevel, computed from level *)
let computeDeclaringFrameAddress (decLevel, level) =
  let rec findFrame currLevel currAddr =
    let {parent; frame=currFrame; _} = currLevel in
    if levelEq decLevel currLevel then
      (* we are in the correct frame *)
      currAddr
    else
      (* the frame we want is further up the chain *)
      let slAccess = match Frame.formals currFrame with
        hd :: _ -> hd
      | [] -> ErrorMsg.impossible "found a frame with no formals - there should always be at least one formal for the static link"
      in
      let parentAddr = Frame.exp slAccess currAddr in
      let parentLevel = match parent with
        Some level -> level
      | None -> ErrorMsg.impossible "followed static links to the outermost level. this should never happen"
      in
      findFrame parentLevel parentAddr
  in
  findFrame level (T.TEMP Frame.fp)

let terminate code =
  T.CALL (
    T.NAME (Temp.namedlabel "exit"),
    [T.CONST code]
  )

let outermost = 
  let frame = Frame.newFrame{name=Temp.newlabel (); formals=[]} in
  {parent=None; frame=frame; eq=ref ()}

let newLevel {parent; name; formals} =
  (* add static link *)
  let frame = Frame.newFrame {name=name; formals=true::formals} in
  {parent=Some parent; frame=frame; eq=ref ()}

let formals level =
  (* omit static link *)
  let {frame; _} = level in
  let sl, params = match Frame.formals frame with
    [] -> ErrorMsg.impossible "there should be at least one formal in the frame for the static link"
  | sl::params -> sl, params
  in
  List.map (fun x -> (level, x)) params

let allocLocal ({frame; _} as level) escapes = (level, Frame.allocLocal frame escapes)

let procEntryExit {level; body} =
  let bodyWithRv = T.MOVE (T.TEMP (Frame.rv), unEx body) in
  let {frame; _} = level in
  let bodyWithViewShift = Frame.procEntryExit1 (frame, bodyWithRv) in
  addFrag (Frame.PROC {body=bodyWithViewShift; frame=frame})

let print exp = Printtree.printtree (stdout, unNx exp)

let printFrag = function
  Frame.STRING (lbl, str) -> print_endline ("STRING(" ^ (Symbol.name lbl) ^ ", " ^ str ^ ")")
| Frame.PROC {body; _} -> Printtree.printtree (stdout, body)

(* translation functions *)
let simpleVar ((decLevel, access), level) =
  let declaringFrameAddr = computeDeclaringFrameAddress (decLevel, level) in
  Ex (Frame.exp access declaringFrameAddr)

let fieldVar (baseAddr, offset) =
  let ebaseAddr = unEx baseAddr in
  let t, f = Temp.newlabel (), Temp.newlabel () in
  Ex (
    T.ESEQ (
      (* if baseAddr evaluates to null pointer, abort *)
      seq [
        T.CJUMP (T.EQ, ebaseAddr, T.CONST 0, t, f);
        T.LABEL t;
        T.EXP (terminate ErrorMsg.null_pointer_error);
        T.LABEL f
      ],
      T.MEM (add ebaseAddr (T.CONST (offset * Frame.wordSize)))
    )
  )

let subscriptVar (baseAddr, offset) =
  (* first word of array is length - use that to bounds-check at runtime *)
  let ebaseAddr = unEx baseAddr in
  let eoffset = unEx offset in
  let a, b, c = Temp.newlabel (), Temp.newlabel (), Temp.newlabel () in
  Ex (
    T.ESEQ (
      seq [
        (* if offset >= 0 *)
        T.CJUMP (T.GE, eoffset, T.CONST 0, a, b);
        T.LABEL a;
        (* and offset < length *)
        T.CJUMP (T.LT, eoffset, T.MEM ebaseAddr, c, b);
        T.LABEL b;
        (* abort if either of those two checks fails *)
        T.EXP (terminate ErrorMsg.array_out_of_bounds_error);
        T.LABEL c
      ],
      T.MEM (add ebaseAddr (mult eoffset (T.CONST Frame.wordSize)))
    )
  )

let nilExp () = Ex (T.CONST 0)

let intExp num =
  if num >= 0 then
    Ex (T.CONST num)
  else
    Ex (sub (T.CONST 0) (T.CONST (-1 * num)))

let stringExp str =
  let eqExisting = function
    Frame.STRING (_, str') -> str = str'
  | _ -> false
  in
  let label = match List.find_opt eqExisting (!frags) with
    Some (Frame.STRING (label, _)) -> label
  | _ ->
    let newLabel = Temp.newlabel () in
    let _ = addFrag (Frame.STRING (newLabel, str)) in
    newLabel
  in
  Ex (T.NAME label)


let callExp (label, funLevel, callLevel, args, hasReturnValue) =
  let ({parent; _}): level = funLevel in
  let unExArgs = List.map unEx args in
  (* address of parent frame is the static link for this function call *)
  let parentLevel = match parent with
    Some (parentLevel) -> parentLevel
  | None -> ErrorMsg.impossible "called a function with no enclosing parent level"
  in
  let call = 
    let declaringFrameAddr = computeDeclaringFrameAddress (parentLevel, callLevel) in
    T.CALL (T.NAME label, declaringFrameAddr :: unExArgs)
  in
  if hasReturnValue then
    Ex call
  else
    Nx (T.EXP call)

let opExp (op, left, right) =
  let leftEx = unEx left in
  let rightEx = unEx right in
  if isRelop op then (* relop *)
    Cx (fun (t,f) -> T.CJUMP (relopFor op, leftEx, rightEx, t, f))
  else (* binop *)
    Ex (T.BINOP (binopFor op, leftEx, rightEx))

let recordExp inits =
  (* malloc(0) could return nullptr depending on implementation.
     in order to get a unique pointer so that equality checks in Tiger work
     even for empty record types, we malloc at least one byte *)
  let size = max ((List.length inits) * Frame.wordSize) 1 in
  let addr = Temp.newtemp () in
  let mallocCall = Frame.externalCall ("malloc", [T.CONST size])
  in
  if inits = [] then
    Ex (
      mallocCall
    )
  else
    (* recursive function to initialize a block of memory with the given fields *)
    let rec initFields fields offset = match fields with
      [] -> ErrorMsg.impossible "should not be initializing fields for an empty record type"
    | [last] -> T.MOVE (
        T.MEM (add (T.TEMP addr) (T.CONST (offset * Frame.wordSize))),
        unEx last
      )
    | hd::tl -> T.SEQ (
        T.MOVE (
          T.MEM (add (T.TEMP addr) (T.CONST (offset * Frame.wordSize))),
          unEx hd
        ),
        initFields tl (offset+1)
      )
    in
    let initTree = initFields inits 0 in
    Ex (
      T.ESEQ (
        T.SEQ (
          T.MOVE (
            T.TEMP addr,
            mallocCall
          ),
          initTree
        ),
        T.TEMP addr
      )
    )

let seqExp lst =
  (* use the first n-1 exp's as statements, then yield result of nth exp *)
  match List.rev lst with
    [] -> Nx (T.EXP (T.CONST 0))
  | [single] -> Ex (unEx single)
  | last :: revHd ->
      let hd = List.rev revHd in (* un-reverse first n-1 *)
      let nHd = List.map unNx hd in (* convert them to Tree.stm *)
      let first = seq nHd in (* construct seq of stms *)
      (match last with
        Nx stm -> Nx (T.SEQ (first, stm))
      | _ -> Ex (T.ESEQ (first, unEx last)))

let assignExp (lvalue, rvalue) =
  let elvalue = unEx lvalue in
  let ervalue = unEx rvalue in
  (* sanity check lvalue *)
  let rec checkLValue = function
    T.MEM _ | T.TEMP _ -> ()
  | T.ESEQ (_, last) -> checkLValue last
  | _ -> ErrorMsg.impossible "Found something other than a MEM or TEMP tree as lvalue in AssignExp"
  in
  let _ = checkLValue elvalue in
  Nx (T.MOVE (elvalue, ervalue))

let ifExp (cond, then', elseopt) =
  let c = unCx cond in
  let t, f = Temp.newlabel (), Temp.newlabel () in
  match then', elseopt with
    _, None ->
    (* if the else branch is none, if statement yields no result *)
    let nthen = unNx then' in
    Nx (seq [
      c (t, f);
      T.LABEL t;
      nthen;
      T.LABEL f;
    ])
  | Nx nthen, Some (Nx nelse) ->
    (* if both branches yield no result, simply evaluate the correct one and yield 0 *)
    let exit = Temp.newlabel () in
    Nx (seq [
      c (t, f);
      T.LABEL t;
      nthen;
      T.JUMP (T.NAME exit, [exit]);
      T.LABEL f;
      nelse;
      T.JUMP (T.NAME exit, [exit]);
      T.LABEL exit
    ])
  | Cx _, Some (else') | _, Some ((Cx _) as else') ->
    (* if either branch is conditional, create a new conditional
         if cond jumps to t, creates a conditional out of then'
         if cond jumps to f, creates a conditional out of else' *)
    let cthen = unCx then' in
    let celse = unCx else' in
    Cx (fun (t', f') -> seq [
      c (t,f);
      T.LABEL t;
      cthen (t', f');
      T.LABEL f;
      celse (t', f')
    ])
  | _, Some (else') ->
    (* by default, treat branches as Ex *)
    let ethen = unEx then' in
    let eelse = unEx else' in
    let exit = Temp.newlabel () in
    let result = Temp.newtemp () in
    Ex (T.ESEQ (
      seq [
        c (t, f);
        T.LABEL t;
        T.MOVE (T.TEMP result, ethen);
        T.JUMP (T.NAME exit, [exit]);
        T.LABEL f;
        T.MOVE (T.TEMP result, eelse);
        T.JUMP (T.NAME exit, [exit]);
        T.LABEL exit
      ],
      T.TEMP result
    ))

let whileExp (cond, body, exit) =
  let c = unCx cond in
  let nbody = unNx body in
  let test, bodyLabel = Temp.newlabel (), Temp.newlabel () in
  Nx (seq [
    T.LABEL test;
    c (bodyLabel, exit);
    T.LABEL bodyLabel;
    nbody;
    T.JUMP (T.NAME test, [test]);
    T.LABEL exit
  ])

let breakExp label =
  Nx (T.JUMP (T.NAME label, [label]))

let arrayExp (size, init) =
  Ex (
    Frame.externalCall ("initArray", [unEx size; unEx init])
  )

let varDec (access, rvalue) =
  (* variable declaration will be in current frame *)
  let (_, frameAccess) = access in
  let lvalue = Frame.exp frameAccess (T.TEMP Frame.fp) in
  Nx (T.MOVE (lvalue, unEx rvalue))
