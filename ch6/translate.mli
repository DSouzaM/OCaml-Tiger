type exp = unit
type level
type access

type newLevelParams = {parent: level; name: Temp.label; formals: bool list}

val outermost: level
val newLevel: newLevelParams -> level
val formals: level -> access list
val allocLocal: level -> bool -> access