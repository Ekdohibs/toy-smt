
type 'a t
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> 'a t
val length : 'a t -> int
val create : int -> 'a -> 'a t
val init : int -> (int -> 'a) -> 'a t
