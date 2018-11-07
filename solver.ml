
open Common

type atom =
  | Equal of int * int
  | Different of int * int

exception Impossible

(*
  A state is represented as a triplet
  (uf, ne, r) where:
  - uf is an union-find structure, that holds the
    equality relations
  - ne is an array that maps each root of the union-find
    structure to the set of the identifier of all the
    clauses x <> y, where either x or y belong to the childs
    of that node
  - r is the first free identifier for <> clauses.

  This allows to answer all queries in O(alpha(n) + log(r)) time,
  where n is the number of variables and r the number of <> clauses.
  Due to how persistant arrays work, backtracking is amortized O(1),
  but only if the state before backtracking is not used again.
*)
type state = Union_find.t * ISet.t Parray.t * int

let create n =
  (Union_find.create n, Parray.create n ISet.empty, 0)

let add_equal ((uf, ne, r) as st) x y =
  let p1 = Union_find.find uf x in
  let p2 = Union_find.find uf y in
  if p1 = p2 then
	st
  else
	let nuf = Union_find.union uf p1 p2 in
	let np = Union_find.find nuf p1 in
	let ne1 = Parray.get ne p1 in
	let ne2 = Parray.get ne p2 in
	if ISet.is_empty (ISet.inter ne1 ne2) then
	  (nuf, Parray.set ne np (ISet.union ne1 ne2), r)
	else
	  raise Impossible

let add_unequal (uf, ne, r) x y =
  let p1 = Union_find.find uf x in
  let p2 = Union_find.find uf y in
  if p1 = p2 then
	raise Impossible
  else
	let ne1 = Parray.get ne p1 in
	let ne2 = Parray.get ne p2 in
	(uf, Parray.set (Parray.set ne p1 (ISet.add r ne1)) p2
	  (ISet.add r ne2), r + 1)

let add_atom st = function
  | Equal (x, y) -> add_equal st x y
  | Different (x, y) -> add_unequal st x y

let known_equal (uf, ne, r) x y =
  let p1 = Union_find.find uf x in
  let p2 = Union_find.find uf y in
  p1 = p2

let known_unequal (uf, ne, r) x y =
  let p1 = Union_find.find uf x in
  let p2 = Union_find.find uf y in
  if p1 = p2 then
	false
  else
	let ne1 = Parray.get ne p1 in
	let ne2 = Parray.get ne p2 in
	not (ISet.is_empty (ISet.inter ne1 ne2))

let known_true st = function
  | Equal (x, y) -> known_equal st x y
  | Different (x, y) -> known_unequal st x y

let known_false st = function
  | Equal (x, y) -> known_unequal st x y
  | Different (x, y) -> known_equal st x y

let negate = function
  | Equal (x, y) -> Different (x, y)
  | Different (x, y) -> Equal (x, y)
	
let build_model (uf, ne, _) =
  let n = Parray.length ne in
  let s = Array.make n [] in
  for i = 0 to n - 1 do
	let c = Union_find.find uf i in
	s.(c) <- i :: s.(c)
  done;
  List.filter ((<>) []) (Array.to_list s)
