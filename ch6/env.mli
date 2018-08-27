type access
type ty = Types.ty
type enventry = VarEntry of {access: Translate.access; ty: ty; const: bool}
              | FunEntry of {level: Translate.level; label: Temp.label; formals: ty list; result: ty}
val base_tenv : ty Symbol.table
val base_venv : enventry Symbol.table