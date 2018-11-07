let enable_debug = ref false
let debug x =
  if !enable_debug then
	Format.eprintf x 
  else
	Format.ifprintf Format.err_formatter x
let protect f =
  if !enable_debug then
	f ()
  else
	try f () with
	| _ -> begin Format.eprintf "Internal compiler error@."; exit 2 end
