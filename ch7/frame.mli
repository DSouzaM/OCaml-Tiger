type frame
type access
type newFrameParams = {name: Temp.label; formals: bool list}
type frag = PROC of {body: Tree.stm; frame: frame}
          | STRING of Temp.label * string

val newFrame: newFrameParams -> frame
val name: frame -> Temp.label
val formals: frame -> access list
val allocLocal: frame -> bool -> access

val fp: Temp.temp
val rv: Temp.temp
val wordSize: int

(* converts an access within the given frame to IR tree representation *)
val exp: access -> Tree.exp -> Tree.exp

(* calls an external runtime function *)
val externalCall: string * Tree.exp list -> Tree.exp

(* implements the "view shift":
    1. move formal parameters
    2. save callee-save registers used in the body
    3. restore callee-save registers after body *)
val procEntryExit1: frame * Tree.stm -> Tree.stm