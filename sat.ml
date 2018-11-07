
open Common

type literal = Solver.atom

type cnf = literal list list

type known = Solver.state
(* The literal we used to get here, and the state before we added it *)
type backjumps = (literal * known) list

exception Solved of known
exception Unsat

let add_known lit known =
  Solver.add_atom known lit

let print_lit ff = function
  | Solver.Equal (x, y) -> Format.fprintf ff "%d = %d" x y
  | Solver.Different (x, y) -> Format.fprintf ff "%d <> %d" x y

let literal_negate = Solver.negate

let literal_known_true known lit = Solver.known_true known lit
let literal_known_false known lit = Solver.known_false known lit
  
let disj_known_true known l =
  List.exists (literal_known_true known) l

let disj_known_false known l =
  List.for_all (literal_known_false known) l

let cnf_known_true known cnf =
  List.for_all (disj_known_true known) cnf

let cnf_known_false known cnf =
  List.exists (disj_known_false known) cnf

(*
	
let next_state cnf (known, backjumps) =
  if cnf_known_true known cnf then
	raise (Solved known)
  else if cnf_known_false known cnf then
	match backjumps with
	| (lit, kn) :: bcjs ->
	   (* Format.printf "Backjump@."; *)
	   (add_known (literal_negate lit) kn, bcjs)
	| [] -> raise Unsat
  else
	(* Unit *)
	let rec unit_decision cl =
	  match cl with
	  | [] -> None
	  | lt :: cls when literal_known_true known lt -> None
	  | lt :: cls when literal_known_false known lt -> unit_decision cls
	  | lt :: cls when disj_known_false known cls -> Some lt
	  | _ -> None
	in
	let rec find_decide_lit l =
	  match l with
	  | [] -> raise Not_found
	  | cl :: ls -> try
		              literal_negate (List.find (fun lit ->
					    not ((literal_known_true known lit) ||
						     (literal_known_false known lit))) cl)
		with Not_found -> find_decide_lit ls
	in
	let rec unit l =
	  match l with
	  | cls :: ls -> begin match unit_decision cls with
		| None -> unit ls
		| Some lit ->
		   (* Format.printf "Unit %a@." print_lit lit; *)
		   (add_known lit known, backjumps)
	    end
	  | [] -> let lit = find_decide_lit cnf in
			  (* Format.printf "Decide %a@." print_lit lit; *)
			  (add_known lit known, (lit, known) :: backjumps)
	in unit cnf

let cnf_solve nvars cnf =
  let rec loop state = loop (next_state cnf state) in
  try loop (Solver.create nvars, []) with
  | Unsat -> None
  | Solved known -> Some (Solver.build_model known)

*)
	 
module UMap = Map.Make (struct
  type t = int * int
  let compare (a, b) (c, d) = compare (min a b, max a b) (min c d, max c d)
end)

let get_pair = function
  | Solver.Equal (x, y) | Solver.Different (x, y) -> (min x y, max x y)
  
type state = ISet.t * (literal * literal) Parray.t * ISet.t UMap.t * Solver.state * backjump
and backjump = Backjump of literal * state | Empty

let get lit m =
  try UMap.find (get_pair lit) m
  with Not_found -> ISet.empty

let add lit i m =
  UMap.add (get_pair lit) (ISet.add i (get lit m)) m
	
let rec recheck acnf ((remaining_clauses, watched, watchset, known, backjumps) as st) found = function
  | [] -> (found, st)
  | rc :: rcs when not (ISet.mem rc remaining_clauses) -> recheck acnf st found rcs
  | rc :: rcs ->
	 let lit1, lit2 = Parray.get watched rc in
	 let mark_solved () =
	   (* Format.printf "%d marked solved@." rc; *)
	   let c1 = get lit1 watchset in
	   let c2 = get lit2 watchset in
	   let nw = UMap.add (get_pair lit1) (ISet.remove rc c1)
		 (UMap.add (get_pair lit2) (ISet.remove rc c2) watchset) in
	   (ISet.remove rc remaining_clauses, watched, nw, known, backjumps)
	 in
	 if literal_known_true known lit1 || literal_known_true known lit2 then
	   recheck acnf (mark_solved ()) true rcs
	 else
	   let l1, l2 = literal_known_false known lit1, literal_known_false known lit2 in
	   if l1 || l2 || lit1 = lit2 then
		 let pair1, pair2 = get_pair lit1, get_pair lit2 in
		 let al, w = if l1 then if l2 then [], 2 else [lit2], 1 else [lit1], 1 in
		 let rec find found rem dj =
		   if rem = 0 then (false, found)
		   else match dj with
		   | [] -> (false, found)
		   | lit :: djs ->
			  if literal_known_true known lit then
				(true, [])
			  else if literal_known_false known lit then
				find found rem djs
			  else
				let p = get_pair lit in
				if not (List.mem p (List.map get_pair found)) then
				  find (lit :: found) (rem - 1) djs
				else
				  find found rem djs
		 in
		 let (one_true, new_watched) = find al w acnf.(rc) in
		 if one_true then
		   recheck acnf (mark_solved ()) true rcs
		 else
		   let c1 = get lit1 watchset in
		   let c2 = get lit2 watchset in
		   let nw = UMap.add pair1 (ISet.remove rc c1)
			 (UMap.add pair2 (ISet.remove rc c2) watchset)
		   in
		   match new_watched with
		   | [] -> begin match backjumps with
			 | Backjump (lt, (remaining_clauses, watched, watchset, known, backjumps)) ->
				(* Format.printf "Backjump@."; *)
				recheck acnf (remaining_clauses, watched,
						 watchset, add_known (literal_negate lt) known, backjumps) true
				  ((ISet.elements (get lt watchset)) @ rcs)
			 | Empty -> raise Unsat
		   end
		   | [lt] -> (* Unit *)
			  (* Format.printf "Unit %d@." rc; *)
			  recheck acnf (ISet.remove rc remaining_clauses, watched,
					   nw, add_known lt known, backjumps) true
				((ISet.elements (get lt nw)) @ rcs)
		   | [lt1; lt2] -> (* Watch changed *)
			  let cc1 = get lt1 nw in
			  let cc2 = get lt2 nw in
			  let nnw = UMap.add (get_pair lt1) (ISet.add rc cc1)
				(UMap.add (get_pair lt2) (ISet.add rc cc2) nw)
			  in
			  recheck acnf (remaining_clauses, Parray.set watched rc (lt1, lt2),
					   nnw, known, backjumps) true rcs
		   | _ -> assert false (* Impossible *)
	   else
		 recheck acnf st found rcs
	
let next_state acnf ((remaining_clauses, watched, watchset, known, backjumps) as st) =
  if ISet.is_empty remaining_clauses then
	raise (Solved known)
  else
	let (found, newstate) = recheck acnf st false (ISet.elements remaining_clauses) in
	if found then
	  newstate
	else (* Nothing new: decide *)
	  let cl = ISet.choose remaining_clauses in
	  let lit1, lit2 = Parray.get watched cl in
	  let l = literal_negate lit1 in
	  snd (recheck acnf ((remaining_clauses, watched, watchset, add_known l known,
					 Backjump (l, st))) false [cl])

let cnf_solve nvars cnf =
  let rec simplify_disj d cur =
	match d with
	| [] -> Some cur
	| Solver.Equal (x, y) :: ds when x = y -> None
	| Solver.Different (x, y) :: ds when x = y -> simplify_disj ds cur
	| ((Solver.Equal (x, y) | Solver.Different (x, y)) as d) :: ds ->
	   let w = match d with
		 | Solver.Equal (x, y) -> Solver.Equal (min x y, max x y)
		 | Solver.Different (x, y) ->Solver.Different (min x y, max x y)
	   in
	   let nw = literal_negate w in
	   if List.mem nw cur then None
	   else if List.mem w cur then simplify_disj ds cur
	   else simplify_disj ds (w :: cur)
  in
  let simp = List.map (fun d -> simplify_disj d []) cnf in
  let acnf = Array.of_list (List.extract simp) in
  if List.mem (Some []) simp then
	None
  else
	let rec build_initial ((remaining_clauses, watched, watchset, known, backjumps) as st) i =
	  if i < Array.length acnf then
		build_initial (
		  match acnf.(i) with
		  | [] -> assert false
		  | [lit] -> (ISet.add i remaining_clauses, Parray.set watched i (lit, lit),
					  add lit i watchset, add_known lit known, backjumps)
		  | lit1 :: lit2 :: _ ->
			 (ISet.add i remaining_clauses, Parray.set watched i (lit1, lit2),
			  add lit1 i (add lit2 i watchset),
			  known, backjumps)
		) (i + 1)
	  else
		st
	in
	let rec loop state = loop (next_state acnf state) in
	let known = Solver.create nvars in
	let watched = Parray.create (Array.length acnf) (Solver.Equal (-1, -1), Solver.Equal (-1, -1)) in
	try loop (build_initial (ISet.empty, watched, UMap.empty, known, Empty) 0) with
	| Unsat -> None
	| Solved known -> Some (Solver.build_model known)

