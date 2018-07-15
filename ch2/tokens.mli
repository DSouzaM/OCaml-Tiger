type linenum = int
type token = string
val _TYPE:  linenum * linenum -> token
val _VAR:  linenum * linenum -> token
val _FUNCTION:  linenum * linenum -> token
val _BREAK:  linenum * linenum -> token
val _OF:  linenum * linenum -> token
val _END:  linenum * linenum -> token
val _IN:  linenum * linenum -> token
val _NIL:  linenum * linenum -> token
val _LET:  linenum * linenum -> token
val _DO:  linenum * linenum -> token
val _TO:  linenum * linenum -> token
val _FOR:  linenum * linenum -> token
val _WHILE:  linenum * linenum -> token
val _ELSE:  linenum * linenum -> token
val _THEN:  linenum * linenum -> token
val _IF:  linenum * linenum -> token
val _ARRAY:  linenum * linenum -> token
val _ASSIGN:  linenum * linenum -> token
val _OR:  linenum * linenum -> token
val _AND:  linenum * linenum -> token
val _GE:  linenum * linenum -> token
val _GT:  linenum * linenum -> token
val _LE:  linenum * linenum -> token
val _LT:  linenum * linenum -> token
val _NEQ:  linenum * linenum -> token
val _EQ:  linenum * linenum -> token
val _DIVIDE:  linenum * linenum -> token
val _TIMES:  linenum * linenum -> token
val _MINUS:  linenum * linenum -> token
val _PLUS:  linenum * linenum -> token
val _DOT:  linenum * linenum -> token
val _RBRACE:  linenum * linenum -> token
val _LBRACE:  linenum * linenum -> token
val _RBRACK:  linenum * linenum -> token
val _LBRACK:  linenum * linenum -> token
val _RPAREN:  linenum * linenum -> token
val _LPAREN:  linenum * linenum -> token
val _SEMICOLON:  linenum * linenum -> token
val _COLON:  linenum * linenum -> token
val _COMMA:  linenum * linenum -> token
val _STRING: (string) *  linenum * linenum -> token
val _INT: (int) *  linenum * linenum -> token
val _ID: (string) *  linenum * linenum -> token
val _EOF:  linenum * linenum -> token