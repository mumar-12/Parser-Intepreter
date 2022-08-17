open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

(*Lil benzo truck *)

let tokenize input = 
    let keywords = ["true"; "false"; "not"; "if"; "then"; "else"; "let"; "def"; "in"; "rec"; "fun"] in
    let length = String.length input in
    let rec helper pos = 
        if pos >= length then []
        else if (Str.string_match (Str.regexp " ") input pos) then 
            (helper (pos+1))
        else if (Str.string_match (Str.regexp "\"[^\"]*\"") input pos) then
                let matched_string = Str.matched_string input in
                let string_length = String.length matched_string in 
                Tok_String (String.sub matched_string 1 (string_length - 2))::(helper (pos + String.length matched_string))
        else if((Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) && 
                 not(List.mem (Str.matched_string input) keywords))then
                let matched_id = Str.matched_string input in
                Tok_ID (matched_id)::(helper (pos + String.length matched_id))
        else if (Str.string_match (Str.regexp "true\\|false") input pos) then
            let matched_bool = Str.matched_string input in
            Tok_Bool (bool_of_string matched_bool)::(helper (pos+ String.length matched_bool))
        else if (Str.string_match (Str.regexp "[0-9]+") input pos) then
            let matched_num = Str.matched_string input in 
            let string_length = String.length matched_num in 
            if (Str.string_match (Str.regexp "[a-zA-Z]") input (pos+string_length)) 
            then raise (InvalidInputException (String.sub input pos (length - pos)))
            else Tok_Int (int_of_string matched_num)::(helper (pos + String.length matched_num))
        else if (Str.string_match (Str.regexp "(-[0-9]+)") input pos) then
            let matched_num = Str.matched_string input in 
            let string_length = String.length matched_num in 
            if (Str.string_match (Str.regexp "[a-zA-Z0-9]") input (pos+string_length)) 
            then raise (InvalidInputException (String.sub input pos (length - pos)))
            else Tok_Int (int_of_string (String.sub matched_num 1 (string_length - 2)))::(helper (pos + string_length))
        else if (Str.string_match (Str.regexp "(") input pos) then
            Tok_LParen::(helper (pos+1))
        else if (Str.string_match (Str.regexp ")") input pos) then
            Tok_RParen::(helper (pos+1))
        else if (Str.string_match (Str.regexp "=") input pos) then
            Tok_Equal::(helper (pos+1))
        else if (Str.string_match (Str.regexp "<>") input pos) then
            Tok_NotEqual::(helper (pos+2))
        else if (Str.string_match (Str.regexp ">=") input pos) then
            Tok_GreaterEqual::(helper (pos+2))
        else if (Str.string_match (Str.regexp "<=") input pos) then
            Tok_LessEqual::(helper (pos+2))
        else if (Str.string_match (Str.regexp ">") input pos) then
            Tok_Greater::(helper (pos+1))
        else if (Str.string_match (Str.regexp "<") input pos) then
            Tok_Less::(helper (pos+1))
        else if (Str.string_match (Str.regexp "||") input pos) then
            Tok_Or::(helper (pos+2))
        else if (Str.string_match (Str.regexp "&&") input pos) then
            Tok_And::(helper (pos+2))
        else if (Str.string_match (Str.regexp "not") input pos) then
            Tok_Not::(helper (pos+3))
        else if (Str.string_match (Str.regexp "if") input pos) then
            Tok_If::(helper (pos+2))
        else if (Str.string_match (Str.regexp "then") input pos) then
            Tok_Then::(helper (pos+4))
        else if (Str.string_match (Str.regexp "else") input pos) then
            Tok_Else::(helper (pos+4))
        else if (Str.string_match (Str.regexp "+") input pos) then
            Tok_Add::(helper (pos+1))
        else if (Str.string_match (Str.regexp "->") input pos) then
            Tok_Arrow::(helper (pos+2))
        else if (Str.string_match (Str.regexp "-") input pos) then
            Tok_Sub::(helper (pos+1))
        else if (Str.string_match (Str.regexp "*") input pos) then
            Tok_Mult::(helper (pos+1))
        else if (Str.string_match (Str.regexp "/") input pos) then
            Tok_Div::(helper (pos+1))
        else if (Str.string_match (Str.regexp "\\^") input pos) then
            Tok_Concat::(helper (pos+1))
        else if (Str.string_match (Str.regexp "let") input pos) then
            Tok_Let::(helper (pos+3))
        else if (Str.string_match (Str.regexp "def") input pos) then
            Tok_Def::(helper (pos+3))
        else if (Str.string_match (Str.regexp "in") input pos) then
            Tok_In::(helper (pos+2))
        else if (Str.string_match (Str.regexp "rec") input pos) then
            Tok_Rec::(helper (pos+3))
        else if (Str.string_match (Str.regexp "fun") input pos) then
            Tok_Fun::(helper (pos+3))
        else if (Str.string_match (Str.regexp ";;") input pos) then
            Tok_DoubleSemi::(helper (pos+2))
        else raise (InvalidInputException (String.sub input pos (length - pos)))
        in helper 0
(*        
let tokenize_try input = 
    let length = String.length input in
        let rec helper pos = 
            if pos >= length then []
            else if (Str.string_match (Str.regexp " ") input pos) then 
                (helper (pos+1))
            else if (Str.string_match (Str.regexp "\"[^\"]*\"") input pos) then
                let matched_string = Str.matched_string input in
                let string_length = String.length matched_string in 
                Tok_String (String.sub matched_string 1 (string_length - 2))::(helper (pos + String.length matched_string))
            else if (Str.string_match (Str.regexp "true\\|false") input pos) then
                let matched_bool = Str.matched_string input in
                Tok_Bool (bool_of_string matched_bool)::(helper (pos+ String.length matched_bool))
            else if (Str.string_match (Str.regexp "[0-9]+") input pos) then
                let matched_num = Str.matched_string input in 
                Tok_Int (int_of_string matched_num)::(helper (pos + String.length matched_num))
            else if (Str.string_match (Str.regexp "(-[0-9]+)") input pos) then
                let matched_num = Str.matched_string input in 
                let string_length = String.length matched_num in 
                Tok_Int (int_of_string (String.sub matched_num 1 (string_length - 2)))::(helper (pos + string_length))
            else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then
                let matched_id = Str.matched_string input in
                Tok_ID (matched_id)::(helper (pos + String.length matched_id))
            else raise (InvalidInputException (String.sub input pos (length - pos)))
            in helper 0

tokenize " "
tokenize_try " "
*)
