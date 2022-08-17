open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks = match (lookahead toks) with
| None -> raise (InvalidInputException "cute, try again....")
| Some token -> match token with 
  | Tok_Let -> parse_Let toks
  | Tok_Fun -> parse_Function toks
  | Tok_If -> parse_If toks
  | _ -> parse_Or toks

and parse_Let (toks: token list) = 
  let tokens_after_operator = match_token toks Tok_Let in 
    match (lookahead tokens_after_operator) with 
    | Some (Tok_ID id) -> parse_Let_helper tokens_after_operator false
    | Some (Tok_Rec) -> let tokens_after_operator = match_token tokens_after_operator Tok_Rec in 
      parse_Let_helper tokens_after_operator true
    | _ -> raise (InvalidInputException "Not rec or id following a let")

and parse_Let_helper (tokens_after_operator: token list) bool_val = 
  match (lookahead tokens_after_operator) with 
      | Some (Tok_ID id) -> let tokens_after_operator = match_token tokens_after_operator (Tok_ID id) in 
        let tokens_after_equal_operator = match_token tokens_after_operator Tok_Equal in
        let (tokens_after_expr, expr) = parse_expr tokens_after_equal_operator in
        let tokens_after_in = match_token tokens_after_expr Tok_In in
        let (tokens_after_2ndexpr, expr2) = parse_expr tokens_after_in in
        (tokens_after_2ndexpr, Let (id, bool_val, expr, expr2))
      | _ -> raise (InvalidInputException "Not rec or id following a let")

and parse_Function (toks: token list) = 
  let tokens_after_fun_operator = match_token toks Tok_Fun in 
  match (lookahead tokens_after_fun_operator) with 
      | Some (Tok_ID id) -> let tokens_after_ID_operator = match_token tokens_after_fun_operator (Tok_ID id) in 
        let tokens_after_arrow_operator = match_token tokens_after_ID_operator Tok_Arrow in 
        let (tokens_after_expr, expr) = parse_expr tokens_after_arrow_operator in
        (tokens_after_expr, Fun(id, expr))
      | _ -> raise (InvalidInputException "Not id following a fun")

and parse_If (toks: token list) = 
  let tokens_after_if_operator = match_token toks Tok_If in 
  let (tokens_after_if_expr, expr) = parse_expr tokens_after_if_operator in
  let tokens_after_then_operator = match_token tokens_after_if_expr Tok_Then in 
  let (tokens_after_then_expr, expr2) = parse_expr tokens_after_then_operator in
  let tokens_after_else_operator = match_token tokens_after_then_expr Tok_Else in 
  let (tokens_after_else_expr, expr3) = parse_expr tokens_after_else_operator in
  (tokens_after_else_expr, If(expr,expr2,expr3))
  
and parse_Or (toks: token list) =
  let (toks_after_and, expr) = parse_And toks in
  match (lookahead toks_after_and) with
  | None -> (toks_after_and, expr)
  | Some token -> match token with 
    | Tok_Or -> let tokens_after_operator = match_token toks_after_and Tok_Or in
        let (tokens_after_or, expr2) = parse_Or tokens_after_operator in
          (tokens_after_or, Binop (Or,expr, expr2)) 
    | _ -> (toks_after_and, expr)


and parse_And (toks: token list) =
  let (toks_after_equality, expr) = parse_Equality toks in
  match (lookahead toks_after_equality) with
  | None -> (toks_after_equality, expr)
  | Some token -> match token with 
    | Tok_And -> let tokens_after_operator = match_token toks_after_equality Tok_And in
        let (tokens_after_and, expr2) = parse_And tokens_after_operator in
          (tokens_after_and, Binop (And,expr, expr2)) 
    | _ -> (toks_after_equality, expr)

and parse_Equality (toks: token list) =
  let (toks_after_relational, expr) = parse_Relational toks in
  match (lookahead toks_after_relational) with
  | None -> (toks_after_relational, expr)
  | Some token -> match token with 
    | Tok_Equal -> let tokens_after_operator = match_token toks_after_relational Tok_Equal in
        let (tokens_after_equality, expr2) = parse_Equality tokens_after_operator in
          (tokens_after_equality, Binop (Equal,expr, expr2)) 
    | Tok_NotEqual -> let tokens_after_operator = match_token toks_after_relational Tok_NotEqual in
        let (tokens_after_equality, expr2) = parse_Equality tokens_after_operator in
          (tokens_after_equality, Binop (NotEqual,expr, expr2))
    | _ -> (toks_after_relational, expr)

and parse_Relational (toks: token list) =
  let (toks_after_additive, expr) = parse_Additive toks in
  match (lookahead toks_after_additive) with
  | None -> (toks_after_additive, expr)
  | Some token -> match token with 
    | Tok_Less -> let tokens_after_operator = match_token toks_after_additive Tok_Less in
        let (tokens_after_relational, expr2) = parse_Relational tokens_after_operator in
          (tokens_after_relational, Binop (Less,expr, expr2)) 
    | Tok_LessEqual -> let tokens_after_operator = match_token toks_after_additive Tok_LessEqual in
        let (tokens_after_relational, expr2) = parse_Relational tokens_after_operator in
          (tokens_after_relational, Binop (LessEqual,expr, expr2))
    | Tok_Greater -> let tokens_after_operator = match_token toks_after_additive Tok_Greater in
        let (tokens_after_relational, expr2) = parse_Relational tokens_after_operator in
          (tokens_after_relational, Binop (Greater,expr, expr2))
    | Tok_GreaterEqual -> let tokens_after_operator = match_token toks_after_additive Tok_GreaterEqual in
        let (tokens_after_relational, expr2) = parse_Relational tokens_after_operator in
          (tokens_after_relational, Binop (GreaterEqual,expr, expr2))
    | _ -> (toks_after_additive, expr)

and parse_Additive (toks: token list) =
  let (toks_after_multiplication, expr) = parse_Multiplicative toks in
  match (lookahead toks_after_multiplication) with
  | None -> (toks_after_multiplication, expr)
  | Some token -> match token with 
    | Tok_Add -> let tokens_after_operator = match_token toks_after_multiplication Tok_Add in
        let (tokens_after_additive, expr2) = parse_Additive tokens_after_operator in
          (tokens_after_additive, Binop (Add,expr, expr2)) 
    | Tok_Sub -> let tokens_after_operator = match_token toks_after_multiplication Tok_Sub in
        let (tokens_after_additive, expr2) = parse_Additive tokens_after_operator in
          (tokens_after_additive, Binop (Sub,expr, expr2))
    | _ -> (toks_after_multiplication, expr)

and parse_Multiplicative (toks: token list) =
  let (toks_after_concat, expr) = parse_Concat toks in
  match (lookahead toks_after_concat) with
  | None -> (toks_after_concat, expr)
  | Some token -> match token with 
    | Tok_Mult -> let tokens_after_operator = match_token toks_after_concat Tok_Mult in
        let (tokens_after_mult, expr2) = parse_Multiplicative tokens_after_operator in
          (tokens_after_mult, Binop (Mult,expr, expr2)) 
    | Tok_Div -> let tokens_after_operator = match_token toks_after_concat Tok_Div in
        let (tokens_after_mult, expr2) = parse_Multiplicative tokens_after_operator in
          (tokens_after_mult, Binop (Div,expr, expr2))
    | _ -> (toks_after_concat, expr)

and parse_Concat (toks: token list) =
  let (toks_after_unary, expr) = parse_Unary toks in
  match (lookahead toks_after_unary) with
  | None -> (toks_after_unary, expr)
  | Some token -> match token with 
    | Tok_Concat -> let tokens_after_operator = match_token toks_after_unary Tok_Concat in
        let (tokens_after_concat, expr2) = parse_Concat tokens_after_operator in
          (tokens_after_concat, Binop (Concat,expr, expr2)) 
    | _ -> (toks_after_unary, expr)

and parse_Unary (toks: token list) =
  match (lookahead toks) with
  | None -> let (tokens_after_functionCall, expr2) = parse_functionCall toks in (tokens_after_functionCall, expr2)
  | Some token -> match token with 
    | Tok_Not -> let tokens_after_operator = match_token toks Tok_Not in
        let (tokens_after_unary, expr) = parse_Unary tokens_after_operator in
          (tokens_after_unary, Not(expr)) 
    | _ -> let (tokens_after_functionCall, expr2) = parse_functionCall toks in (tokens_after_functionCall, expr2)

and parse_functionCall (toks: token list) =
  let (toks_after_primary, expr) = parse_PrimaryExpr toks in
  match (lookahead toks_after_primary) with
  | None -> (toks_after_primary, expr)
  | Some token -> match token with 
    | Tok_Int value -> let (toks_after_second_primary, expr2) = parse_PrimaryExpr toks_after_primary in
        (toks_after_second_primary, FunctionCall (expr, expr2)) 
    | Tok_Bool value -> let (toks_after_second_primary, expr2) = parse_PrimaryExpr toks_after_primary in
        (toks_after_second_primary, FunctionCall (expr, expr2)) 
    | Tok_String value -> let (toks_after_second_primary, expr2) = parse_PrimaryExpr toks_after_primary in
        (toks_after_second_primary, FunctionCall (expr, expr2)) 
    | Tok_ID value -> let (toks_after_second_primary, expr2) = parse_PrimaryExpr toks_after_primary in
        (toks_after_second_primary, FunctionCall (expr, expr2)) 
    | Tok_LParen -> let (toks_after_second_primary, expr2) = parse_PrimaryExpr toks_after_primary in
        (toks_after_second_primary, FunctionCall (expr, expr2)) 
    | _ -> (toks_after_primary, expr)

and parse_PrimaryExpr (toks: token list) = match lookahead toks with 
  | None -> raise (InvalidInputException "Pain, u suck...")
  | Some (Tok_Int value) -> (match_token toks (Tok_Int value), Value (Int(value)))
  | Some (Tok_Bool value) -> (match_token toks (Tok_Bool value), Value (Bool (value)))
  | Some (Tok_String value) -> (match_token toks (Tok_String value), Value (String (value)))
  | Some (Tok_ID value) -> (match_token toks (Tok_ID value), (ID (value)))
  | Some (Tok_LParen) -> (let rest_of_toks = match_token toks Tok_LParen in 
      match parse_expr rest_of_toks with 
      | (remaining_tokens, expr) -> let tokens_after_R_Paren = match_token remaining_tokens Tok_RParen in
        (tokens_after_R_Paren, expr))
  | _ -> raise (InvalidInputException "ERROR ERROR ERROR")



(*  Part 3: Parsing mutop *)
let rec parse_mutop toks = 
match (lookahead toks) with
| None -> raise (InvalidInputException "cute, try again....")
| Some token -> match token with 
  | Tok_DoubleSemi -> ([], NoOp)
  | Tok_Def -> parse_Def toks
  | _ -> let (tokens_remaining, expr ) = parse_expr toks in 
    match (lookahead tokens_remaining) with 
    | Some Tok_DoubleSemi -> let tokens_after_semi = match_token tokens_remaining Tok_DoubleSemi in
      (tokens_after_semi, Expr(expr))
    | _ -> raise (InvalidInputException "There wasn't a Tok_DoubleSemi after the expr")

and parse_Def (toks: token list) = 
  let tokens_after_def_operator = match_token toks Tok_Def in 
  match (lookahead tokens_after_def_operator) with 
      | Some (Tok_ID id) -> let tokens_after_ID_operator = match_token tokens_after_def_operator (Tok_ID id) in 
        let tokens_after_equal_operator = match_token tokens_after_ID_operator Tok_Equal in 
        let (tokens_after_expr, expr) = parse_expr tokens_after_equal_operator in
        let tokens_after_semi = match_token tokens_after_expr Tok_DoubleSemi in
        (tokens_after_semi, Def(id, expr))
      | _ -> raise (InvalidInputException "Not id following a def")
