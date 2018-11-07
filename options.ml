
let usage = "usage: esmt [options] file.cnfuf"

let rec spec =
  [
	"-G", Arg.Set Debug.enable_debug, "  show debug messages";
	"-h", Arg.Unit (fun () -> Arg.usage spec usage; exit 0),
	  "  Display this list of options";
  ]
