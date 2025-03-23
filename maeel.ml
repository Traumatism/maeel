type 'a stack = 'a Stack.t
type ('a, 'b) hashtbl = ('a, 'b) Hashtbl.t

type tokenkind =
  | Symbol of char
  | Name of string
  | Int of int
  | Float of float
  | Str of string
  | Block of token list
  | End

and token = tokenkind * int

type obj =
  | Ostr of string
  | Oint of int
  | Ofloat of float
  | Olist of obj list
  | Oblock of token list

let rec string_of_obj = function
  | Ostr s -> "\"" ^ s ^ "\""
  | Oint i -> string_of_int i
  | Ofloat f -> string_of_float f
  | Olist lst -> "[" ^ String.concat "; " (List.map string_of_obj lst) ^ "]"
  | Oblock token_list -> failwith "can't convert obj(block) to string"

let ( <+> ) (lhs : obj) (rhs : obj) : obj option =
  match (lhs, rhs) with
  | Ostr a, Ostr b -> Some (Ostr (a ^ b))
  | Oint a, Oint b -> Some (Oint (a + b))
  | Ofloat a, Ofloat b -> Some (Ofloat (a +. b))
  | Ofloat f, Oint i | Oint i, Ofloat f -> Some (Ofloat (f +. float_of_int i))
  | o, Olist xs | Olist xs, o -> Some (Olist (o :: xs))
  | _ -> None

let ( <*> ) (lhs : obj) (rhs : obj) : obj option =
  match (lhs, rhs) with
  | Oint a, Oint b -> Some (Oint (a * b))
  | Ofloat a, Ofloat b -> Some (Ofloat (a *. b))
  | Ofloat f, Oint i | Oint i, Ofloat f -> Some (Ofloat (f *. float_of_int i))
  | _ -> None

let ( <<> ) (lhs : obj) (rhs : obj) : obj =
  let result =
    match (lhs, rhs) with
    | Oint a, Oint b -> a < b
    | Ofloat a, Ofloat b -> a < b
    | _ -> false
  in
  if result then Oblock [ (Name "drop", -1) ]
  else Oblock [ (Name "drop", -1); (Name "swap", -1) ]

let ( <=> ) (lhs : obj) (rhs : obj) : obj =
  let result =
    match (lhs, rhs) with
    | Oint a, Oint b -> a = b
    | Ofloat a, Ofloat b -> a = b
    | Ostr a, Ostr b -> a = b
    | Olist xs, Olist ys -> xs = ys
    | _ -> false
  in
  if result then Oblock [ (Name "drop", -1) ]
  else Oblock [ (Name "drop", -1); (Name "swap", -1) ]

let opp : obj -> obj option = function
  | Oint x -> Some (Oint (-x))
  | Ofloat x -> Some (Ofloat (-.x))
  | _ -> None

let inv : obj -> obj option = function
  | Oint x -> Some (Oint (1 / x))
  | Ofloat x -> Some (Ofloat (1. /. x))
  | _ -> None

let tokenize (code : string) : token stack =
  let line = ref 1
  and tokens : token list ref = ref []
  and chars : char list ref = ref (code |> String.to_seq |> List.of_seq) in

  let rec parse_chars () : unit =
    let rec take_while (pred : char -> bool) (acc : string) : string =
      match !chars with
      | c :: tl when pred c ->
          let () = chars := tl in
          take_while pred (acc ^ String.make 1 c)
      | _ -> acc
    and is_alpha : char -> bool = function
      | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false
    and is_digit : char -> bool = function '0' .. '9' -> true | _ -> false
    and ( $ ) (obj : 'a) (xs : 'a list ref) : unit = xs := obj :: !xs in

    match !chars with
    | [] -> ()
    | c :: tl ->
        let () = chars := tl in
        let () =
          match c with
          | '\n' -> incr line
          | ' ' | '\t' -> ()
          | '#' ->
              let _ = take_while (fun c -> c <> '\n') "" in
              incr line
          | '"' ->
              let rec aux (acc : string) : string =
                match !chars with
                | c :: tl when c <> '"' ->
                    let () = chars := tl in
                    aux (acc ^ String.make 1 c)
                | _ -> acc
              in
              (Str (aux ""), !line) $ tokens
          | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
              let content =
                take_while
                  (fun c -> is_digit c || is_alpha c || c = '_')
                  (String.make 1 c)
              in
              (Name content, !line) $ tokens
          | '0' .. '9' ->
              let content =
                take_while (fun c -> is_digit c || c = '.') (String.make 1 c)
              in
              let tokenized =
                if String.contains content '.' then
                  Float (float_of_string content)
                else Int (int_of_string content)
              in
              (tokenized, !line) $ tokens
          | _ -> (Symbol c, !line) $ tokens
        in

        parse_chars ()
  in

  let () = parse_chars () in
  let rec parse_blocks (tokens : token list) (stack : token list list)
      (tmp : token list) (acc : token stack) : token stack =
    match tokens with
    | [] ->
        let () = List.iter (fun x -> Stack.push x acc) tmp in
        acc
    | (Symbol '(', _) :: tl -> parse_blocks tl (tmp :: stack) [] acc
    | (Symbol ')', _) :: tl -> (
        let block = (Block tmp, !line) in
        match stack with
        | x :: tl' -> parse_blocks tl tl' (block :: x) acc
        | [] ->
            let () = Stack.push block acc in
            parse_blocks tl stack [] acc)
    | h :: tl -> parse_blocks tl stack (h :: tmp) acc
  in
  parse_blocks (List.rev !tokens) [] [] (Stack.create ())

and run_tokens (tokens_stack : token stack) : unit =
  let vm_stack = Stack.create ()
  and vm_variables_stack = Stack.create ()
  and vm_funs = Hashtbl.create 128 in

  let extend xs = List.iter (fun token -> Stack.push token tokens_stack) xs
  and vm_push o = Stack.push o vm_stack
  and vm_pop () = Stack.pop vm_stack
  and vm_unwrap (line : int) (msg : string) (x : obj option) : obj =
    match x with
    | Some y -> y
    | None ->
        let () = Printf.printf "line %d: %s" line msg in
        exit 1
  in

  let () = Stack.push (Hashtbl.create 128) vm_variables_stack in

  while not (Stack.is_empty tokens_stack) do
    let token, line = Stack.pop tokens_stack in
    match token with
    | End ->
        let _ = Stack.pop vm_variables_stack in
        ()
    | Str content -> Ostr content |> vm_push
    | Int content -> Oint content |> vm_push
    | Float content -> Ofloat content |> vm_push
    | Block content -> Oblock content |> vm_push
    | Symbol '+' ->
        vm_pop () <+> vm_pop () |> vm_unwrap line "failed to add" |> vm_push
    | Symbol '*' ->
        vm_pop () <*> vm_pop ()
        |> vm_unwrap line "failed to multiply"
        |> vm_push
    | Symbol '/' ->
        vm_pop ()
        <*> (inv (vm_pop ()) |> vm_unwrap line "failed to inverse")
        |> vm_unwrap line "failed to divide"
        |> vm_push
    | Symbol '-' ->
        vm_pop ()
        <+> (opp (vm_pop ()) |> vm_unwrap line "failed to oppo")
        |> vm_unwrap line "failed to substract"
        |> vm_push
    | Symbol '=' -> vm_pop () <=> vm_pop () |> vm_push
    | Symbol '<' -> vm_pop () <<> vm_pop () |> vm_push
    | Symbol ':' ->
        let identifier =
          match Stack.pop tokens_stack with
          | Name name, _ -> name
          | _ -> failwith "tried to assign but nothing on the stack"
        in
        let value = vm_pop () in
        Hashtbl.add (Stack.top vm_variables_stack) identifier value
    | Symbol '!' -> (
        match vm_pop () with
        | Oblock tokens -> extend tokens
        | _ -> failwith "tried to execute something that can't be executed")
    | Name "let" ->
        let rec parse_vars (vars : string list) : string list =
          match Stack.pop tokens_stack with
          | Name "in", _ -> vars
          | Name name, line -> parse_vars (name :: vars)
          | _ -> failwith "invalid syntax when defining function"
        in
        let vars = parse_vars [] in
        List.iter
          (fun var ->
            let value = vm_pop () in
            Hashtbl.add (Stack.top vm_variables_stack) var value)
          vars
    | Name "end" ->
        let _ = Stack.pop vm_variables_stack in
        if Stack.is_empty vm_variables_stack then
          let () = Stack.push (Hashtbl.create 128) vm_variables_stack in
          ()
    | Name "fn" ->
        let name =
          match Stack.pop tokens_stack with
          | Name content, _ -> content
          | _ -> failwith "expected a name after keyword fun"
        in

        let rec parse_params (params : token list) : token list * token list =
          match Stack.pop tokens_stack with
          | (Name _ as token'), line ->
              parse_params ((Symbol ':', line) :: (token', line) :: params)
          | Block tokens', _ -> (params, tokens')
          | _ -> failwith "invalid syntax when defining function"
        in
        let params, block = parse_params [] in
        Hashtbl.add vm_funs name (block @ List.rev params)
    | Name "list" -> Olist [] |> vm_push
    | Name "puts" -> vm_pop () |> string_of_obj |> print_endline
    | Symbol '~' -> (
        match vm_pop () with
        | Olist [] -> vm_push (Olist [])
        | Olist (x :: xs) ->
            let () = vm_push (Olist xs) in
            let () = vm_push x in
            ()
        | _ -> failwith "i can only deconstruct a list")
    | Name identifier -> (
        try Hashtbl.find (Stack.top vm_variables_stack) identifier |> vm_push
        with Not_found -> (
          try
            let () = Stack.push (End, -1) tokens_stack in
            let () =
              Stack.push
                (Hashtbl.copy (Stack.top vm_variables_stack))
                vm_variables_stack
            in
            Hashtbl.find vm_funs identifier |> extend
          with Not_found ->
            failwith (Printf.sprintf "unbound identifier: %s" identifier)))
    | Symbol c -> Printf.sprintf "unexpected symbol: %c" c |> failwith
  done
;;

if Array.length Sys.argv <> 2 then failwith "Please provide a .maeel file"
else
  let ch = open_in_bin Sys.argv.(1) in
  let content = in_channel_length ch |> really_input_string ch in
  let () = close_in ch in
  content |> tokenize |> run_tokens
