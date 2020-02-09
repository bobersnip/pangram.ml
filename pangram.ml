(* Your program will consist of a function that accepts two strings. 
	Each string is the name of a file.
	The first is the name of an input file and the second is the name of an output file. 
	Name the function pangram. *)
	
	
(* read a all the lines from a file *)
(* taken from lab2 *)
let rec readlines' (ch : in_channel) : string list = 
  match input_line ch with
    | s -> s :: readlines' ch
	| exception  End_of_file -> [] (* input_line throws an exception at the end of the file *)	
;;

(* channels can be hard so make a function that always opens and closes them correctly *)
(* taken from lab2 *)
let readlines (path : string) : string list =
	let ch = open_in path in
	let lines = readlines' ch in
	let _ = close_in ch in
	lines
;;

(* write many lines to a file *)
(* taken from lab2 *)
let rec writelines (path : string) (ls : string list ) : unit =
  let rec loop (ch) (ls : string list ) : unit =
    match ls with
      | [] -> ()
	  | x :: xs -> let _ = Printf.fprintf ch "%s\n" x in loop ch xs
  in 
  let ch = open_out path in
  let ()  = loop ch ls in
  let () = close_out ch in
  ()
;;

let pangram (paths:string * string) : unit =
	let (input,output) = paths in
	let in_strings = readlines input in	
	(* checker function for one line *)
	let rec check (str:string) (check_char:int) : bool =
		if (check_char < 123)
		then 
			if String.contains (str) (Char.chr check_char)
			then check (str) (check_char+1)
			else false
		else true		
	in
	(* function that runs through each line, checking for pangram *)
	let rec line_check (strings:string list) : string list =
		match strings with
			| [] -> []
			| line :: rest -> Bool.to_string (check (String.lowercase_ascii line) (97)) :: (line_check rest)
	in
	writelines output (line_check in_strings)
