%{

  open Lexing
  open Kawa

%}

%token <int> INT
%token TRUE FALSE
%token <string> IDENT
%token PLUS
%token MAIN
%token LPAR RPAR BEGIN END SEMI
%token PRINT
%token EOF

%left PLUS

%start program
%type <Kawa.program> program

%%

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
;

expression:
| n=INT { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| e1=expression PLUS e2=expression { Binop(Add, e1, e2) }
;
