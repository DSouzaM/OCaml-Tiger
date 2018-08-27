type venv
type tenv
type env = {venv: venv; tenv: tenv}
type expty

val transVar: venv * tenv * Translate.level * Absyn.var -> expty
val transExp: venv * tenv * Translate.level * Absyn.exp -> expty
val transDec: venv * tenv * Translate.level * Absyn.dec -> env
val transTy:         tenv * Absyn.ty  -> Types.ty
val transProg:              Absyn.exp -> unit

exception SemanticError