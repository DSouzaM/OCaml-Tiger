type venv
type tenv
type env = {venv: venv; tenv: tenv}
type expty

val transVar: venv * tenv * Translate.level * Temp.label * Absyn.var -> expty
val transExp: venv * tenv * Translate.level * Temp.label * Absyn.exp -> expty
val transDec: venv * tenv * Translate.level * Temp.label * Absyn.dec -> env * Translate.exp list
val transTy:         tenv * Absyn.ty  -> Types.ty
val transProg:              Absyn.exp -> Translate.frag list

exception SemanticError