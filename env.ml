(* define Env.t *)
type ('a, 'b) t = ('a * 'b) list

(* empty environment *)
let empty = []

(* get value from environment *)
let rec get (env : ('a, 'b) t) (var : 'a) : 'b = match env with
  | [] -> raise Not_found
  | (a, b) :: rest -> if a = var then b else get rest var

(* extend environment *)
let extend (env : ('a, 'b) t) (var : 'a) (value : 'b) : ('a, 'b) t =
  (var, value) :: env
