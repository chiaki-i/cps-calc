(* Target.t : type for target language, in Continuation-Passing Style.
 * Target language is basically STLC *)
type t =
  | Number of int
  | Bool of bool
  | Var of string
  | CPSLam of string * t
  | MetaLam of string * t
  | CPSApp of t * t
  | MetaApp of t * t

(* convert target language into text *)
let rec to_string (exp : t) : string = match exp with
  | Number (n) -> string_of_int n
  | Bool (b) -> string_of_bool b
  | Var (x) -> x
  | CPSLam (x, e) -> "(^λ " ^ x ^ ". " ^ (to_string e) ^ ")"
  | MetaLam (x, e) -> "(_λ " ^ x ^ ". " ^ (to_string e) ^ ")"
  | CPSApp (e1, e2) -> "(" ^ (to_string e1) ^ " ^@ " ^ (to_string e2) ^ ")"
  | MetaApp (e1, e2) -> "(" ^ (to_string e1) ^ " _@ " ^ (to_string e2) ^ ")"

(* convert target language into LaTeX : TODO *)
(* let u_lambda = "\underline{\lambda}"
 * let o_lambda = "\overline{\lambda}" *)
(* let rec to_latex (exp : t) : string = match exp with
 *   | Number (n) -> string_of_int n
 *   | Bool (b) -> string_of_bool b
 *   | Var (x) -> x
 *   | CPSLam (x, e) -> "( " ^ x ^ ". " ^ (to_string e) ^ ")"
 *   | MetaLam (x, e) -> "(_λ " ^ x ^ ". " ^ (to_string e) ^ ")"
 *   | CPSApp (e1, e2) -> "(" ^ (to_string e1) ^ " ^@ " ^ (to_string e2) ^ ")"
 *   | MetaApp (e1, e2) -> "(" ^ (to_string e1) ^ " _@ " ^ (to_string e2) ^ ")" *)

(* print result *)
let print (exp : t) : unit =
  let str = to_string exp in
  print_string str
