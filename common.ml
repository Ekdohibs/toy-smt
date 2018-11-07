module List = struct
  include List
  let iter3 f l m n =
	iter2 (fun a (b,c) -> f a b c) l (combine m n)
  let map3 f l m n =
	map2 (fun a (b,c) -> f a b c) l (combine m n)
  let iteri2 f l m =
	iteri (fun i (a,b) -> f i a b) (combine l m)
  let mapi2 f l m =
	mapi (fun i (a,b) -> f i a b) (combine l m)
  let rec fold_map f e l =
	match l with
	| [] -> e, []
	| x :: t -> let e', y = f e x in
				let e'', t' = fold_map f e' t in
				e'', y :: t'
  let fold_mapi f e l =
	let rec aux i e l =
	  match l with
	  | [] -> e, []
	  | x :: t -> let e', y = f i e x in
				  let e'', t' = aux (i + 1) e' t in
				  e'', y :: t'
	in aux 0 e l
  let range a b =
	let rec aux a b r =
	  if a >= b then r
	  else aux a (b - 1) ((b - 1) :: r)
	in aux a b []
  let rec extract = function
	| None :: ls -> extract ls
	| Some x :: ls -> x :: (extract ls)
	| [] -> []
end

let rec print_list ff f sep l =
  match l with
	[] -> ()
  | [x] -> f ff x
  | x :: t ->
	 f ff x;
	 Format.fprintf ff sep;
	 print_list ff f sep t

module ISet = Set.Make(struct type t = int let compare = compare end)
