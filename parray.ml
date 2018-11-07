
type 'a t = 'a data ref
and 'a data = Array of 'a array | Diff of 'a t * int * 'a

let rec get_array_reroot a =
  match !a with
  | Array arr -> arr
  | Diff (u, index, value) ->
	 let w = get_array_reroot u in
	 let old_value = w.(index) in
	 w.(index) <- value;
	 a := Array w;
	 u := Diff (a, index, old_value);
	 w

let get a i =
  (get_array_reroot a).(i)
	
let set a i v =
  let w = get_array_reroot a in
  let old_value = w.(i) in
  w.(i) <- v;
  let result = ref (Array w) in
  a := Diff (result, i, old_value);
  result

let length a =
  Array.length (get_array_reroot a)
	
let create n v = ref (Array (Array.make n v))
  
let init n f = ref (Array (Array.init n f))
