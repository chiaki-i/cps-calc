type source_t =
  | Number of int
  | Bool of bool
  | Var of string
  | Lambda of string * source_t
  | App of source_t * source_t
  | Shift of string * source_t
  | Reset of source_t

let rec to_string (exp : source_t) : string = match exp with
  | Number (n) -> string_of_int n
  | Bool (b) -> string_of_bool b
  | Var (x) -> x
  | Lambda (x, e) -> "(λ " ^ x ^ ". " ^ (to_string e) ^ ")"
  | App (e1, e2) -> "(" ^ (to_string e1) ^ " @ " ^ (to_string e2) ^ ")"
  | Shift (k, e) -> "(S " ^ k ^ ". " ^ (to_string e) ^ ")"
  | Reset (e) -> "⟨" ^ (to_string e) ^ "⟩"

