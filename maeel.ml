type token =
  | Symbol of char
  | Name of string
  | Int of int
  | Float of float
  | Str of string
  | Block of token_data list

and token_data = token * string * int

type obj = Ostr of string | Oint of int | Ofloat of float | Olist of obj list

let add lhs rhs =
  match (lhs, rhs) with
  | Ostr a, Ostr b -> Ostr (a ^ b)
  | Oint a, Oint b -> Oint (a + b)
  | Ofloat a, Ofloat b -> Ofloat (a +. b)
  | Ofloat f, Oint i | Oint i, Ofloat f -> Ofloat (f +. float_of_int i)
  | _ -> failwith "undefined"

let mul lhs rhs =
  match (lhs, rhs) with
  | Oint a, Oint b -> Oint (a * b)
  | Ofloat a, Ofloat b -> Ofloat (a *. b)
  | Ofloat f, Oint i | Oint i, Ofloat f -> Ofloat (f *. float_of_int i)
  | _ -> failwith "undefined"

let opp obj =
  match obj with
  | Oint x -> Oint (-x)
  | Ofloat x -> Ofloat (-.x)
  | _ -> failwith "undefined"

let inv obj =
  match obj with
  | Oint x -> Oint (1 / x)
  | Ofloat x -> Ofloat (1. /. x)
  | _ -> failwith "undefined"

let lex_into_tokens code file_name =
  let depth = ref 0 in
  let line = ref 1 in
  let tokens = ref [] in
  let chars = ref (List.of_seq (String.to_seq code)) in

  let rec parse_chars () =
    let rec take_while pred acc =
      match !chars with
      | [] -> acc
      | c :: tl when pred c ->
          chars := tl;
          take_while pred (acc ^ String.make 1 c)
      | _ -> acc
    in

    let is_alpha alpha =
      let code = Char.code alpha in
      (Char.code 'a' <= code && code <= Char.code 'z')
      || (Char.code 'A' <= code && code <= Char.code 'Z')
    in

    let is_digit digit =
      let code = Char.code digit in
      Char.code '0' <= code && code <= Char.code '9'
    in

    match !chars with
    | [] -> ()
    | c :: tl ->
        chars := tl;
        (match c with
        | '\n' -> incr line
        | ' ' | '\t' -> ()
        | '#' ->
            let _ = take_while (fun c -> c <> '\n') "" in
            incr line
        | '(' | ')' ->
            tokens := (Symbol c, file_name, !line) :: !tokens;
            depth := !depth + if c = '(' then 1 else -1
        | '"' ->
            let content = take_while (fun c -> c <> '"') "" in
            tokens := (Str content, file_name, !line) :: !tokens
        | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
            let content =
              take_while
                (fun c -> is_digit c || is_alpha c || c = '_')
                (String.make 1 c)
            in
            tokens := (Name content, file_name, !line) :: !tokens
        | '0' .. '9' ->
            let content =
              take_while (fun c -> is_digit c || c = '.') (String.make 1 c)
            in
            let tokenized =
              if String.contains content '.' then
                Float (float_of_string content)
              else Int (int_of_string content)
            in
            tokens := (tokenized, file_name, !line) :: !tokens
        | _ -> tokens := (Symbol c, file_name, !line) :: !tokens);

        parse_chars ()
  in
  parse_chars ();
  assert (!depth = 0);

  let rec parse_blocks tokens stack tmp_tokens output =
    match tokens with
    | [] ->
        let rec aux xs =
          match xs with
          | [] -> output
          | x :: tl ->
              Stack.push x output;
              aux tl
        in
        aux tmp_tokens
    | (Symbol '(', _, _) :: tl ->
        parse_blocks tl (tmp_tokens :: stack) [] output
    | (Symbol ')', _, _) :: tl -> (
        let block = (Block tmp_tokens, file_name, !line) in
        match stack with
        | x :: tl' -> parse_blocks tl tl' (block :: x) output
        | [] ->
            Stack.push block output;
            parse_blocks tl stack [] output)
    | token :: tl -> parse_blocks tl stack (token :: tmp_tokens) output
  in
  parse_blocks (List.rev !tokens) [] [] (Stack.create ())

let run_tokens tokens_stack =
  let rec aux tokens_stack vm_stack vm_variables vm_funs =
    let vm_push obj = Stack.push obj vm_stack in
    let vm_pop () = Stack.pop vm_stack in
    let vm_binop binop = binop (vm_pop ()) (vm_pop ()) |> vm_push in

    while not (Stack.is_empty tokens_stack) do
      let token, file, line = Stack.pop tokens_stack in
      match token with
      | Str content -> Ostr content |> vm_push
      | Int content -> Oint content |> vm_push
      | Float content -> Ofloat content |> vm_push
      | Symbol '+' -> add |> vm_binop
      | Symbol '*' -> mul |> vm_binop
      | Symbol '/' -> (fun x y -> mul x (inv y)) |> vm_binop
      | Symbol '-' -> (fun x y -> add x (opp y)) |> vm_binop
      | Symbol ':' ->
          let identifier, _, _ = Stack.pop tokens_stack in
          let value = vm_pop () in
          Hashtbl.add vm_variables identifier value
      | Name identifier -> (
          match identifier with
          | "fun" ->
              let name, _, _ = Stack.pop tokens_stack in
              let rec parse_params params =
                let token', file, line = Stack.pop tokens_stack in
                match token' with
                | Name _ ->
                    parse_params
                      ((token', file, line) :: (Symbol ':', file, line)
                     :: params)
                | Block tokens' -> (params, tokens')
                | _ -> failwith "invalid syntax when defining function"
              in
              let params, block = parse_params [] in
              Hashtbl.add vm_funs name (block @ params)
          | "drop" ->
              let _ = vm_pop () in
              ()
          | "list" -> Olist [] |> vm_push
          | "puts" ->
              let output =
                match vm_pop () with
                | Ostr content -> content
                | Oint content -> string_of_int content
                | Ofloat content -> string_of_float content
                | Olist _ -> failwith "..."
              in
              Printf.printf "%s" output
          | _ -> (
              try
                let value = Hashtbl.find vm_variables (Name identifier) in
                vm_push value
              with Not_found -> (
                try
                  let _ = Hashtbl.find vm_funs (Name identifier) in
                  ()
                with Not_found -> failwith "unbound identifier")))
      | _ -> ()
    done
  in
  aux tokens_stack (Stack.create ()) (Hashtbl.create 128) (Hashtbl.create 128)
;;

let ch = open_in_bin "examples.maeel" in
let content = in_channel_length ch |> really_input_string ch in
close_in ch;
lex_into_tokens content "" |> run_tokens
