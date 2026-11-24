{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "print",    PRINT;
      "main",     MAIN;

      "var",      VAR;
      "int",      INT_TYPE;
      "bool",     BOOL_TYPE;
      "void",     VOID_TYPE;

      "true",     TRUE;
      "false",    FALSE;
      "if",       IF;
      "else",     ELSE;

      "while",    WHILE;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let number = digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  (* whitespace *)
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  (* comments *)
  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  (* keywords and identifiers *)
  | number as n  { INT(int_of_string n) }
  | ident as id  { keyword_or_ident id }

  | "=" { ASSIGN }
  (* binary operators *)
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { STAR }
  | "/"  { DIV }
  | "%"  { MOD }
  | "=="  { EQ }
  | "!=" { NEQ }
  | "!"  { NOT }
  | "<"  { LT }
  | "<=" { LE }
  | ">"  { GT }
  | ">=" { GE }
  | "&&" { AND }
  | "||" { OR }

  (* punctuation *)
  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  
  (* end of file *)
  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
