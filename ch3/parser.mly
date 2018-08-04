%{
  open ErrorMsg

  let error tokidx msg =
    let pos = Parsing.rhs_start tokidx in
    let errmsg = "Syntax error: " ^ msg in
    ErrorMsg.error pos errmsg

  let badStr typ where =
    let pad = if (String.length where) = 0 then "" else " in " in
    typ ^ pad ^ where ^ " improperly formed."
  let badExpr where = badStr "Expression" where
  let badDecl where = badStr "Declaration(s)" where
%}

%token EOF 
%token <string> ID STRING
%token <int> INT
%token COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE DOT
%token PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE AND OR ASSIGN ARRAY
%token IF THEN ELSE WHILE FOR TO DO LET IN END OF BREAK
%token NIL FUNCTION VAR TYPE

%nonassoc DO THEN OF
%nonassoc ELSE
%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT GT GE LE GEQ LEQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start program
%type <unit> program

%%

program	:
    exp EOF {}
  ;

exp :
    /* control constructs */
    LPAREN exp RPAREN {}
  | LPAREN exp SEMICOLON exp expseq_ RPAREN {}
  | IF exp THEN exp {}
  | IF exp THEN exp ELSE exp {}
  | WHILE exp DO exp {}
  | FOR ID ASSIGN exp TO exp DO exp {}
  | LET decs IN expseq END {}
  | /* simple expressions */
    lvalue {}
  | LPAREN RPAREN {}
  | NIL {}
  | INT {}
  | MINUS exp %prec UMINUS {}
  | STRING {}
  | /* binary operations */
    exp PLUS exp {}
  | exp MINUS exp {}
  | exp TIMES exp {}
  | exp DIVIDE exp {}
  | exp EQ exp {}
  | exp NEQ exp {}
  | exp LT exp {}
  | exp LE exp {}
  | exp GT exp {}
  | exp GE exp {}
  | exp AND exp {}
  | exp OR exp {}
  | /* more complicated expressions */
    ID LPAREN argseq RPAREN {}
  | ID LBRACE fieldseq RBRACE {}
  | ID LBRACK exp RBRACK OF exp {}
  | lvalue ASSIGN exp {}
  | /* error expressions */
    LPAREN error RPAREN {error 2 (badExpr "parentheses")}
  | IF error THEN exp {error 2 (badExpr "if-then condition")}
  | WHILE error DO exp {error 2 (badExpr "while condition")}
  | FOR ID ASSIGN error TO exp DO exp {error 4 (badExpr "for loop initial value")}
  | FOR ID ASSIGN exp TO error DO exp {error 6 (badExpr "for loop final value")}
  | LET error IN expseq END {error 2 (badDecl "let-block")}  
  | error {error 1 (badExpr "")}
  ;

lvalue :
    ID {}
  | lvalue DOT ID {}
  | lvalue LBRACK exp RBRACK {}
  | lvalue LBRACK error RBRACK {error 3 (badExpr "array subscript")}
  | ID LBRACK exp RBRACK {}
  | ID LBRACK error RBRACK {error 3 (badExpr "array subscript")}

  ;

decs :
    /* empty */ {}
  | dec decs {}
  ;

dec : 
    tydec {}
  | vardec {}
  | fundec {}
  ;

tydec :
    TYPE ID EQ ty {}
  ;

ty :
    ID {}
  | LBRACE tyfields RBRACE {}
  | ARRAY OF ID {}
  | LBRACE error RBRACE {error 2 (badDecl "record field list")}
  ;

tyfields :
    /* empty */ {}
  | ID COLON ID tyfields_ {}
  ;

tyfields_ :
    /* empty */ {}
  | COMMA ID COLON ID tyfields_ {}
  ;

vardec :
    VAR ID ASSIGN exp {}
  | VAR ID COLON ID ASSIGN exp {}
  ;

fundec :
    FUNCTION ID LPAREN tyfields RPAREN EQ exp {}
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp {}
  | FUNCTION ID LPAREN error RPAREN EQ exp {error 4 (badDecl "parameter list")}
  | FUNCTION ID LPAREN error RPAREN COLON ID EQ exp {error 4 (badDecl "parameter list")}
  ;

expseq :
    /* empty */ {}
  | exp expseq_ {}
  ;

expseq_ :
    /* empty */ {}
  | SEMICOLON exp expseq_ {} 
  ;

argseq :
    /* empty */ {}
  | exp argseq_ {}
  ;

argseq_ :
    /* empty */ {}
  | COMMA exp argseq_ {}
  ;

fieldseq :
    /* empty */ {}
  | ID EQ exp fieldseq_ {}
  ;

fieldseq_ :
    /* empty */ {}
  | COMMA ID EQ exp fieldseq_ {}
  ;