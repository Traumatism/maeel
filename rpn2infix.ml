let is_operator = function
  | "+" | "-" | "*" | "/" -> true
  | _ -> false

let precedence = function
  | "+" | "-" -> 1
  | "*" | "/" -> 2
  | _ -> 0

let convert_rpn_to_infix rpn =
  let stack = ref [] in
  let tokens = String.split_on_char ' ' rpn in

  let rec process_tokens = function
    | [] -> List.hd !stack
    | token :: rest ->
        if is_operator token then
          let rhs = List.hd !stack in stack := List.tl !stack;
          let lhs = List.hd !stack in stack := List.tl !stack;
          let result = "(" ^ lhs ^ token ^ rhs ^ ")" in stack := result :: !stack;
          process_tokens rest
        else (stack := token :: !stack; process_tokens rest)

  in process_tokens tokens

let rpn_expression = "1 1 + 3 *"
let infix_expression = convert_rpn_to_infix rpn_expression;;

print_endline infix_expression;;