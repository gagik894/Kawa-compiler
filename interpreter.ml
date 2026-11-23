open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null
and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec eval_call f this args =
    failwith "eval_call not implemented"

  and exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false
        
    and eval (e : expr) : value =
      match e with
      | Int n -> VInt n
      | Bool b -> VBool b
      | Binop (op, left, right) -> eval_binop op left right
      | Get (Var name) ->
        (match Hashtbl.find_opt env name with
        | Some value -> value
        | None -> raise (Error ("unknown variable: " ^ name)))
      | _ -> failwith "case not implemented in eval"

    and eval_binop op left right =
      let with_ints f =
        let a = evali left in
        let b = evali right in
        f a b
      in
      let equal () =
        let a = eval left in
        let b = eval right in
        match a, b with
        | VInt x, VInt y -> x = y
        | VBool x, VBool y -> x = y
        | _ -> assert false
      in
      match op with
      | Add -> VInt (with_ints (+))
      | Sub -> VInt (with_ints (-))
      | Mul -> VInt (with_ints ( * ))
      | Div -> VInt (with_ints (/))
      | Rem -> VInt (with_ints (mod))
      | Lt  -> VBool (with_ints (<))
      | Le  -> VBool (with_ints (<=))
      | Gt  -> VBool (with_ints (>))
      | Ge  -> VBool (with_ints (>=))
      | Eq  -> VBool (equal ())
      | Neq -> VBool (not (equal ()))
      | And -> VBool (evalb left && evalb right)
      | Or  -> VBool (evalb left || evalb right)
    in
  
    let rec exec (i: instr): unit = match i with
      | Print e -> Printf.printf "%d\n" (evali e)
      | If (condition, then_branch, else_branch) ->
        if evalb condition then exec_seq then_branch
        else exec_seq else_branch
      | Set (Var name, rhs) ->
        if Hashtbl.mem env name then
          Hashtbl.replace env name (eval rhs)
        else
          raise (Error ("unknown variable: " ^ name))
      | _ -> failwith "case not implemented in exec"
    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in
  
  exec_seq p.main (Hashtbl.create 1)
