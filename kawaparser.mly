%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token TRUE FALSE
%token <string> IDENT
%token PLUS MINUS
%token STAR DIV
%token MOD
%token LT LE GT GE
%token AND OR
%token EQ NEQ
%token NOT

%token IF ELSE
%token WHILE

%token ASSIGN
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token PRINT

%token VAR INT_TYPE BOOL_TYPE VOID_TYPE

%token EOF

%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc LT LE GT GE
%left PLUS MINUS
%left STAR DIV MOD
%nonassoc UNARY

%start program
%type <Kawa.program> program

%%

program:
| globals=list(var_decl) MAIN BEGIN main=list(instruction) END EOF
    { { classes = []; globals; main } }
;

var_decl:
| VAR ty=typ name=IDENT SEMI { (name, ty) }
;

typ:
| INT_TYPE   { TInt }
| BOOL_TYPE  { TBool }
| name=IDENT { TClass name }
| VOID_TYPE  { TVoid }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| x=IDENT ASSIGN e=expression SEMI { Set(Var(x), e) }
| IF LPAR e=expression RPAR BEGIN s1=list(instruction) END ELSE BEGIN s2=list(instruction) END 
    { If(e, s1, s2) } 
| WHILE LPAR e=expression RPAR BEGIN s=list(instruction) END { While(e, s) }
;

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| STAR { Mul }
| DIV { Div }
| MOD { Rem }
| EQ { Eq }
| NEQ { Neq }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| AND { And }
| OR { Or }
;

expression:
| n=INT { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| x=IDENT { Get(Var(x)) }
| MINUS e=expression %prec UNARY { Unop (Opp, e) }
| NOT e=expression %prec UNARY { Unop (Not, e) }
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }
| LPAR e=expression RPAR { e }
;
