open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with 
| Value typeValue -> typeValue
| ID name -> lookup env name 
| Not expr -> (let val1 = eval_expr env expr in
            match val1 with 
            | Bool x -> Bool(not x)
            | _ -> raise (TypeError ("I didn't get a bool for Not")))
| Binop (operator, expr1,expr2) -> (match operator with 
  | Add -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Int x, Int y) -> Int(x + y)
           | _ -> raise (TypeError ("I didn't get two ints for Add")))
  | Sub -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Int x, Int y) -> Int(x - y)
           | _ -> raise (TypeError ("I didn't get two ints for Sub")))
  | Mult -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Int x, Int y) -> Int(x * y)
           | _ -> raise (TypeError ("I didn't get two ints for Mult")))
  | Div -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Int x, Int y) -> if y = 0 then raise (DivByZeroError) else Int(x / y)
           | _ -> raise (TypeError ("I didn't get two ints for Mult")))
  | Less -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Int x, Int y) -> Bool(x<y)
           | _ -> raise (TypeError ("I didn't get two ints for Less")))
  | LessEqual -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Int x, Int y) -> Bool(x<=y)
           | _ -> raise (TypeError ("I didn't get two ints for LessEqual")))
  | Greater -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Int x, Int y) -> Bool(x>y)
           | _ -> raise (TypeError ("I didn't get two ints for Greater")))
  | GreaterEqual -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Int x, Int y) -> Bool(x>=y)
           | _ -> raise (TypeError ("I didn't get two ints for GreaterEqual")))
  | Concat -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (String x, String y) -> String(x^y)
           | _ -> raise (TypeError ("I didn't get two Strings for Concat")))
  | Equal -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (String x, String y) -> Bool(x=y)
           | (Int x, Int y) -> Bool(x=y)
           | (Bool x, Bool y) -> Bool(x=y)
           | _ -> raise (TypeError ("I didn't get two of the same type for Equal")))
  | NotEqual -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (String x, String y) -> Bool(x<>y)
           | (Int x, Int y) -> Bool(x<>y)
           | (Bool x, Bool y) -> Bool(x<>y)
           | _ -> raise (TypeError ("I didn't get two of the same type for NotEqual")))
  | Or -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Bool x, Bool y) -> Bool(x||y)
           | _ -> raise (TypeError ("I didn't get two Bool for Or")))
  | And -> (let val1 = eval_expr env expr1 in
           let val2 = eval_expr env expr2 in
           match (val1, val2) with 
           | (Bool x, Bool y) -> Bool(x&&y)
           | _ -> raise (TypeError ("I didn't get two Bool for And"))))
| If (guard,expr1,expr2) -> (let is_bool = eval_expr env guard in
  match is_bool with 
  | Bool bool_val -> if bool_val then eval_expr env expr1 else eval_expr env expr2
  | _ -> raise (TypeError ("If guard was not a bool")))
| Let (var, recOrNo, init, body) -> (if recOrNo 
  then let temp_env = extend_tmp env var in 
       let temp_val = eval_expr temp_env init in
       let () = update temp_env var temp_val in
       eval_expr temp_env body
  else let new_env = extend env var (eval_expr env init) in eval_expr new_env body)
| Fun (str, expr)-> (Closure (env, str, expr))
| FunctionCall (expr1, expr2) -> 
  (let val2 = eval_expr env expr2 in 
  let val1 = eval_expr env expr1 in 
  match val1 with 
  | Closure (temp_env, str, funcVal) -> let new_env = extend temp_env str val2 in eval_expr new_env funcVal
  | _ -> raise (TypeError ("FunctionCall expr1 was not a Closure")))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with 
| NoOp -> (env, None) 
| Expr expr -> (match eval_expr env expr with 
  | Int int -> (env, Some(Int(int)))
  | Bool bool -> (env, Some(Bool(bool)))
  | String string -> (env, Some(String(string)))
  | Closure (environment, var, expr) -> (environment, Some(Closure(environment, var, expr))))
| Def (var, expr) -> (
  let temp_env = extend_tmp env var in 
  let new_val = eval_expr temp_env expr in
  let () = update temp_env var new_val in 
  match new_val with 
  | Int int -> (temp_env, Some(Int(int)))
  | Bool bool -> (temp_env, Some(Bool(bool)))
  | String string -> (temp_env, Some(String(string)))
  | Closure (_, var, expr) -> (temp_env, Some(Closure(temp_env, var, expr))))


