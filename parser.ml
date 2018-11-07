
exception Parser_error of string

let split s c =
  let rec spl last index =
	if index >= String.length s then
	  [String.sub s last (index - last)]
	else if s.[index] = c then
	  (String.sub s last (index - last)) :: spl (index + 1) (index + 1)
	else
	  spl last (index + 1)
  in spl 0 0

let parse_literal str =
  let rec parse_int i acc =
	if i >= String.length str then (acc, i)
	else let c = str.[i] in
		 if '0' <= c && c <= '9' then
		   let u = int_of_char c - int_of_char '0' in
		   parse_int (i + 1) (10 * acc + u)
		 else
		   (acc, i)
  in
  let (val1, i1) = parse_int 0 0 in
  let (eq, i) = 
	if str.[i1] = '=' then
	  (true, i1 + 1)
	else if str.[i1] = '<' && str.[i1 + 1] = '>' then
	  (false, i1 + 2)
	else
	  raise (Parser_error "Incorrecly formed literal")
  in
  let (val2, i2) = parse_int i 0 in
  if eq then
	Solver.Equal (val1 - 1, val2 - 1)
  else
	Solver.Different (val1 - 1, val2 - 1)
		   
  
let rec parse_cnfs in_channel nvariables nclauses =
  if nclauses = 0 then
	[]
  else
	let line = input_line in_channel in
	if line = "" || line.[0] = 'c' then
	  parse_cnfs in_channel nvariables nclauses
	else
	  let spl = split line ' ' in
	  let spl = List.filter ((<>) "") spl in
	  (List.map parse_literal spl) :: (parse_cnfs in_channel nvariables (nclauses - 1))

let rec parse in_channel =
  let line = input_line in_channel in
  if line.[0] = 'c' then
	parse in_channel
  else if line.[0] = 'p' then
	let spl = split line ' ' in
	if List.length spl <> 4 then
	  raise (Parser_error "Incorrect description line")
	else if List.nth spl 1 <> "cnf" then
	  raise (Parser_error "Formula should be cnf")
	else
	  let nvariables = int_of_string (List.nth spl 2) in
	  let nclauses = int_of_string (List.nth spl 3) in
	  (nvariables, parse_cnfs in_channel nvariables nclauses)
	else
	  raise (Parser_error "Invalid line")
