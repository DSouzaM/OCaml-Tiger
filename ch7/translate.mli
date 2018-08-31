type exp
type level
type access
type frag

type newLevelParams = {parent: level; name: Temp.label; formals: bool list}
type procEntryExitParams = {level: level; body: exp}

val outermost: level
val newLevel: newLevelParams -> level
val formals: level -> access list
val allocLocal: level -> bool -> access
val procEntryExit: procEntryExitParams -> unit
val print: exp -> unit
val printFrag: frag -> unit

val getResult: unit -> frag list
(* clear fragments *)
val reset: unit -> unit

val simpleVar: access * level -> exp

val fieldVar: exp * int -> exp 

val subscriptVar: exp * exp -> exp

val nilExp: unit -> exp

val intExp: int -> exp

val stringExp: string -> exp

val callExp: Temp.label * level * level * exp list * bool -> exp

val opExp: Absyn.oper * exp * exp -> exp

val recordExp: exp list -> exp

val seqExp: exp list -> exp

val assignExp: exp * exp -> exp

val ifExp: exp * exp * exp option -> exp

val whileExp: exp * exp * Temp.label -> exp

val breakExp: Temp.label -> exp

val arrayExp: exp * exp -> exp

val varDec: access * exp -> exp


