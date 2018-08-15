type access
type ty = Types.ty
type enventry = VarEntry of {ty: ty; const: bool}
              | FunEntry of {formals: ty list; result: ty}
val base_tenv : ty Symbol.table
val base_venv : enventry Symbol.table