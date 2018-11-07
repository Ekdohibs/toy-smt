
type t = {
  rank : int Parray.t ;
  mutable parent : int Parray.t ;
}

let create n =
  {
	rank = Parray.create n 1 ;
	parent = Parray.init n (fun x -> x) ;
  }
  
let rec find uf x =
  let p = Parray.get uf.parent x in
  if p = x then x
  else
	let r = find uf p in
	uf.parent <- Parray.set uf.parent x r;
	r

let union uf x y =
  let p1 = find uf x in
  let p2 = find uf y in
  if p1 = p2 then uf
  else
	let r1 = Parray.get uf.rank p1 in
	let r2 = Parray.get uf.rank p2 in
	if r1 < r2 then
	  {
		rank = Parray.set uf.rank p2 (r1 + r2) ;
		parent = Parray.set uf.parent p1 p2 ;
	  }
	else
	  {
		rank = Parray.set uf.rank p1 (r1 + r2) ;
		parent = Parray.set uf.parent p2 p1 ;
	  }
