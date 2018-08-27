type frame
type access
type newFrameParams = {name: Temp.label; formals: bool list}

val newFrame: newFrameParams -> frame
val name: frame -> Temp.label
val formals: frame -> access list
val allocLocal: frame -> bool -> access