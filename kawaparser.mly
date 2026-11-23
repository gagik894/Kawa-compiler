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

%token IF ELSE

%token ASSIGN
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token PRINT
%token EOF

%nonassoc EQ
%left PLUS MINUS
%left STAR DIV

%start program
%type <Kawa.program> program

%%

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
| x=IDENT ASSIGN e=expression SEMI { Set(Var(x), e) }
| IF LPAR e=expression RPAR BEGIN s1=list(instruction) END ELSE BEGIN s2=list(instruction) END 
    { If(e, s1, s2) } 
;

%inline binop:
| PLUS { Add }
| MINUS { Sub }
| STAR { Mul }
| DIV { Div }
| EQ { Eq }
;

expression:
| n=INT { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| x=IDENT { Get(Var(x)) }
| e1=expression op=binop e2=expression { Binop(op, e1, e2) }

;
