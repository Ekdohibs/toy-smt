open Format

let in_chan =
  let file = ref None in
  let set_file s =
    file := Some s
  in
  Arg.parse Options.spec set_file Options.usage;
  match !file with
  | Some "-" -> stdin
  | Some f -> open_in f
  | None ->
	 Arg.usage Options.spec Options.usage; exit 1
	   
let () = Debug.protect (fun () ->
  let (nvars, cnf) = Parser.parse in_chan in
  let solved = Sat.cnf_solve nvars cnf in
  match solved with
  | None -> Format.printf "No solution@."
  | Some l ->
	 List.iter (fun ll ->
	   Format.printf "%d" (List.hd ll + 1);
	   List.iter (fun x -> Format.printf "=%d" (x + 1)) (List.tl ll);
	   Format.printf "@."
	 ) l
)
