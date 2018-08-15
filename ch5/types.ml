type unique = unit ref

type ty = INT
        | STRING
        | RECORD of (Symbol.symbol * ty) list * unique
        | ARRAY of ty * unique
        | NIL
        | UNIT
        | NAME of Symbol.symbol * ty option ref