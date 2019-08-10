(* Source.t : type for source language, in Direct Style *)
type t =
  | Number of int
  | Bool of bool
  | Var of string
  | Lambda of string * t
  | App of t * t
  | Shift of string * t
  | Reset of t

(* convert source language into string *)
let rec to_string (exp : t) : string = match exp with
  | Number (n) -> string_of_int n
  | Bool (b) -> string_of_bool b
  | Var (x) -> x
  | Lambda (x, e) -> "(λ " ^ x ^ ". " ^ (to_string e) ^ ")"
  | App (e1, e2) -> "(" ^ (to_string e1) ^ " @ " ^ (to_string e2) ^ ")"
  | Shift (k, e) -> "(S " ^ k ^ ". " ^ (to_string e) ^ ")"
  | Reset (e) -> "⟨" ^ (to_string e) ^ "⟩"

(* print user input *)
let print (exp : t) : unit =
  let str = to_string exp in
  print_string str
