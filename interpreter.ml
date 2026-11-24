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

  and exec_seq instructions lenv =
    let rec eval (expression : expr) : value =
      match expression with
      | Int n -> VInt n
      | Bool b -> VBool b
      | Unop (Opp, operand) -> VInt (-(eval_int operand))
      | Unop (Not, operand) -> VBool (not (eval_bool operand))
      | Binop (op, left, right) -> eval_binop op left right
      | Get (Var name) ->
          (match Hashtbl.find_opt env name with
           | Some value -> value
           | None -> raise (Error ("unknown variable: " ^ name)))
      | _ -> failwith "case not implemented in eval"

    and eval_int expression =
      match eval expression with
      | VInt n -> n
      | _ -> assert false

    and eval_bool expression =
      match eval expression with
      | VBool b -> b
      | _ -> assert false

    and eval_obj expression =
      match eval expression with
      | VObj o -> o
      | _ -> assert false

    and eval_binop op left right =
      let with_ints f =
        let a = eval_int left in
        let b = eval_int right in
        f a b
      in
      let equal () =
        let a = eval left in
        let b = eval right in
        match a, b with
        | VInt x, VInt y -> x = y
        | VBool x, VBool y -> x = y
        | VObj x, VObj y -> x == y
        | Null, Null -> true
        | _ -> false
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
      | And -> VBool (eval_bool left && eval_bool right)
      | Or  -> VBool (eval_bool left || eval_bool right)
    in

    let rec exec (instruction : instr) : unit =
      match instruction with
      | Print e -> Printf.printf "%d\n" (eval_int e)
      | If (condition, then_branch, else_branch) ->
          if eval_bool condition then exec_block then_branch
          else exec_block else_branch
      | Set (Var name, rhs) ->
          if Hashtbl.mem env name then
            Hashtbl.replace env name (eval rhs)
          else
            raise (Error ("unknown variable: " ^ name))
      | While (condition, body) ->
          while eval_bool condition do
            exec_block body
          done
      | _ -> failwith "case not implemented in exec"
    and exec_block block =
      List.iter exec block
    in

    exec_block instructions
  in

  exec_seq p.main (Hashtbl.create 1)
