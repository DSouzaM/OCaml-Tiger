%{
  open ErrorMsg
  module A = Absyn

  let newSym name = Symbol.symbol name

  let leftPos tokidx =
    Parsing.rhs_start tokidx

  let left () = leftPos 1

  (* for error productions *)
  let dummyExp = A.NilExp
  let dummyLvalue = A.SimpleVar (newSym "dummy", 0)
  let dummyTy = A.NameTy (newSym "dummy", 0)

  let error tokidx msg =
    let pos = leftPos tokidx in
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
%type <Absyn.exp> program exp

%%

program	:
    exp EOF {$1}
  ;

exp :
    /* control constructs */
    LPAREN exp RPAREN {$2}
  | LPAREN exp SEMICOLON exp expseq_ RPAREN {A.SeqExp (($2, leftPos 2) :: ($4, leftPos 4) :: $5)}
  | IF exp THEN exp {A.IfExp {test=$2; then'=$4; else'=None; pos=left ()}}
  | IF exp THEN exp ELSE exp {A.IfExp {test=$2; then'=$4; else'=Some $6; pos=left ()}}
  | WHILE exp DO exp {A.WhileExp {test=$2; body=$4; pos=left ()}}
  | FOR ID ASSIGN exp TO exp DO exp {A.ForExp {var=newSym $2; escape=ref true; lo=$4; hi=$6; body=$8; pos=left ()}}
  | LET decs IN expseq END {A.LetExp {decs=$2; body=A.SeqExp $4; pos=left ()}}
  | /* simple expressions */
    lvalue {A.VarExp $1}
  | LPAREN RPAREN {A.SeqExp []}
  | NIL {A.NilExp}
  | INT {A.IntExp $1}
  | MINUS exp %prec UMINUS {A.OpExp {left=A.IntExp 0; oper=A.MinusOp; right=$2; pos=left ()}}
  | STRING {A.StringExp ($1, left ())}
  | BREAK {A.BreakExp (left ())}
  | /* binary operations */
    exp PLUS exp {A.OpExp {left=$1; oper=A.PlusOp; right=$3; pos=leftPos 2}}
  | exp MINUS exp {A.OpExp {left=$1; oper=A.MinusOp; right=$3; pos=leftPos 2}}
  | exp TIMES exp {A.OpExp {left=$1; oper=A.TimesOp; right=$3; pos=leftPos 2}}
  | exp DIVIDE exp {A.OpExp {left=$1; oper=A.DivideOp; right=$3; pos=leftPos 2}}
  | exp EQ exp {A.OpExp {left=$1; oper=A.EqOp; right=$3; pos=leftPos 2}}
  | exp NEQ exp {A.OpExp {left=$1; oper=A.NeqOp; right=$3; pos=leftPos 2}}
  | exp LT exp {A.OpExp {left=$1; oper=A.LtOp; right=$3; pos=leftPos 2}}
  | exp LE exp {A.OpExp {left=$1; oper=A.LeOp; right=$3; pos=leftPos 2}}
  | exp GT exp {A.OpExp {left=$1; oper=A.GtOp; right=$3; pos=leftPos 2}}
  | exp GE exp {A.OpExp {left=$1; oper=A.GeOp; right=$3; pos=leftPos 2}}
  | exp AND exp {A.IfExp {test=$1; then'=$3; else'=Some (A.IntExp 0); pos=leftPos 2}}
  | exp OR exp {A.IfExp {test=$1; then'=A.IntExp 1; else'=Some $3; pos=leftPos 2}}
  | /* more complicated expressions */
    ID LPAREN argseq RPAREN {A.CallExp {func=newSym $1; args=$3; pos=left ()}}
  | ID LBRACE fieldseq RBRACE {A.RecordExp {fields=$3; typ=newSym $1; pos=left ()}}
  | ID LBRACK exp RBRACK OF exp {A.ArrayExp {typ=newSym $1; size=$3; init=$6; pos=left ()}}
  | lvalue ASSIGN exp {A.AssignExp {var=$1; exp=$3; pos=leftPos 2}}
  | /* error expressions */
    LPAREN error RPAREN {error 2 (badExpr "parentheses"); dummyExp}
  | IF error THEN exp {error 2 (badExpr "if-then condition"); dummyExp}
  | WHILE error DO exp {error 2 (badExpr "while condition"); dummyExp}
  | FOR ID ASSIGN error TO exp DO exp {error 4 (badExpr "for loop initial value"); dummyExp}
  | FOR ID ASSIGN exp TO error DO exp {error 6 (badExpr "for loop final value"); dummyExp}
  | LET error IN expseq END {error 2 (badDecl "let-block"); dummyExp}
  | error {error 1 (badExpr ""); dummyExp}
  ;

lvalue :
    ID {A.SimpleVar (newSym $1, left ())}
  | lvalue DOT ID {A.FieldVar ($1, newSym $3, leftPos 2)}
  | lvalue LBRACK exp RBRACK {A.SubscriptVar ($1, $3, leftPos 2)}
  | lvalue LBRACK error RBRACK {error 3 (badExpr "array subscript"); dummyLvalue}
  | ID LBRACK exp RBRACK {A.SubscriptVar (A.SimpleVar (newSym $1, left ()), $3, leftPos 2)}
  | ID LBRACK error RBRACK {error 3 (badExpr "array subscript"); dummyLvalue}
  ;

decs :
    /* empty */ {[]}
  | dec decs {$1 :: $2}
  ;

dec : 
    tydec {$1}
  | vardec {$1}
  | fundec {$1}
  ;

tydec :
    TYPE ID EQ ty tydec_ {A.TypeDec ({name=newSym $2; ty=$4; pos=left ()} :: $5)}
  ;

tydec_ :
    /* empty */ {[]}
  | TYPE ID EQ ty tydec_ {{name=newSym $2; ty=$4; pos=left ()} :: $5}

ty :
    ID {A.NameTy (newSym $1, left ())}
  | LBRACE tyfields RBRACE {A.RecordTy $2}
  | ARRAY OF ID {A.ArrayTy (newSym $3, left ())}
  | LBRACE error RBRACE {error 2 (badDecl "record field list"); dummyTy}
  ;

tyfields :
    /* empty */ {[]}
  | ID COLON ID tyfields_ {{name=newSym $1; escape=ref true; typ=newSym $3; pos=leftPos 2} :: $4}
  ;

tyfields_ :
    /* empty */ {[]}
  | COMMA ID COLON ID tyfields_ {{name=newSym $2; escape=ref true; typ=newSym $4; pos=leftPos 3} :: $5}
  ;

vardec :
    VAR ID ASSIGN exp {A.VarDec {name=newSym $2; escape=ref true; typ=None; init=$4; pos=leftPos 2}}
  | VAR ID COLON ID ASSIGN exp {A.VarDec {name=newSym $2; escape=ref true; typ=Some (newSym $4, leftPos 4); init=$6; pos=leftPos 2}}
  ;

fundec :
    FUNCTION ID LPAREN tyfields RPAREN EQ exp fundec_ {A.FunctionDec ({name=newSym $2; params=$4; result=None; body=$7; pos=left ()} :: $8)}
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundec_ {A.FunctionDec ({name=newSym $2; params=$4; result=Some (newSym $7, leftPos 7); body=$9; pos=left ()} :: $10)}
  | FUNCTION ID LPAREN error RPAREN EQ exp fundec_ {error 4 (badDecl "parameter list"); A.FunctionDec $8}
  | FUNCTION ID LPAREN error RPAREN COLON ID EQ exp fundec_ {error 4 (badDecl "parameter list"); A.FunctionDec $10}
  ;

fundec_ :
    /* empty */ {[]}
  | FUNCTION ID LPAREN tyfields RPAREN EQ exp fundec_ {{name=newSym $2; params=$4; result=None; body=$7; pos=left ()} :: $8}
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundec_ {{name=newSym $2; params=$4; result=Some (newSym $7, leftPos 7); body=$9; pos=left ()} :: $10}
  | FUNCTION ID LPAREN error RPAREN EQ exp fundec_ {error 4 (badDecl "parameter list"); $8}
  | FUNCTION ID LPAREN error RPAREN COLON ID EQ exp fundec_ {error 4 (badDecl "parameter list"); $10}
  ;
expseq :
    /* empty */ {[]}
  | exp expseq_ {($1, left ()) :: $2}
  ;

expseq_ :
    /* empty */ {[]}
  | SEMICOLON exp expseq_ {($2, leftPos 2) :: $3}
  ;

argseq :
    /* empty */ {[]}
  | exp argseq_ {$1 :: $2}
  ;

argseq_ :
    /* empty */ {[]}
  | COMMA exp argseq_ {$2 :: $3}
  ;

fieldseq :
    /* empty */ {[]}
  | ID EQ exp fieldseq_ {(newSym $1, $3, leftPos 2) :: $4}
  ;

fieldseq_ :
    /* empty */ {[]}
  | COMMA ID EQ exp fieldseq_ {(newSym $2, $4, leftPos 3) :: $5}
  ;