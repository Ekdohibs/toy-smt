exception Impossible
type atom =
  | Equal of int * int
  | Different of int * int
type state
val create : int -> state
val add_atom : state -> atom -> state
val known_true : state -> atom -> bool
val known_false : state -> atom -> bool
val negate : atom -> atom
val build_model : state -> int list list
