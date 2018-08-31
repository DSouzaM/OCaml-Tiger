module T = Tree

type access = InFrame of int
            | InReg of Temp.temp

type frame = {name: Temp.label; formals: access list; locals: int ref}

type frag = PROC of {body: Tree.stm; frame: frame}
          | STRING of Temp.label * string

let fp = Temp.newtemp ()

let rv = Temp.newtemp ()

let n_param_in_reg = 6

let wordSize = 4

let buildFormalAccesses formals =
  let combine (n_reg, n_frame, lst) = function
  (* if parameter does not escape, and we have another register for the next param, put it in reg *)
    false when n_reg < n_param_in_reg ->
      (n_reg+1, n_frame, InReg(Temp.newtemp ())::lst)
  (* else, just put it in the frame *)
  | _ ->
      (n_reg, n_frame+1, InFrame((n_frame+1)*wordSize)::lst)
  in
  (* build the list from left to right (so in-frame params
    that appear later have higher memory addresses), but the
    resultant list is reversed *)
  let (_, _, reversedAccesses) = List.fold_left combine (0, 0, []) formals in  
  List.rev reversedAccesses

type newFrameParams = {name: Temp.label; formals: bool list}
let newFrame ({name; formals} : newFrameParams) =
  {name=name; formals=buildFormalAccesses formals; locals=ref 0}

let name ({name; _}: frame) = name

let formals ({formals; _}: frame) = formals

let allocLocal ({locals; _}: frame) = function
  true ->
    let _ = locals := !locals + 1 in
    InFrame(!locals * -4)
| false -> InReg(Temp.newtemp ())



let exp access frameAddr = match access with
  InReg t -> Tree.TEMP t
| InFrame offset -> Tree.MEM (Tree.BINOP (Tree.PLUS, frameAddr, Tree.CONST offset))

let externalCall (s, explist) = T.CALL (T.NAME (Temp.namedlabel s), explist)

(* TODO implement *)
let procEntryExit1 (frame, stmts) = stmts
