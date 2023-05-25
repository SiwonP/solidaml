let rec state_1 input =
	let c = input_char input in
	match c with
	(* method *)
	| 'a'..'z' | 'A'..'Z' | '-' (* tchar *) ->
		 state_1 input 
	(* WS *) 
	| ' ' -> 
		state_2 input 
	| _ -> "error1" 
and state_2 input = (* shall recognize the reques-target *)
	let c = input_char input in
	match c with
	(* origin-form *)
	| '/' -> 
		state_3 input 
	(* asterisk-form *) 
	(* | '*' -> state_x input *)
	| _ -> "error2"
and state_3 input = (* absolute-path *)
	let c = input_char input in
	match c with
	(* segment *)
 	| 'a'..'z' | 'A'..'Z' | '0'..'9' (* pchar *) -> 
		state_3 input 
	(* absolute-path *) 
	| '/' -> state_3 input 
	(* query *)
	| '?' -> state_4 input 
	(* request-target is done *)
	| ' ' -> state_5 input 
	| _ -> "error3"
and state_4 input =
	let c = input_char input in
	match c with
	(* query is empty and request_target is done *)
	| ' ' -> state_5 input
	(* query *)
	| 'a'..'z' | 'A'..'Z' | '0'..'9' (* pchar *) ->  
			state_4 input 
	(* query *)
	| '?' -> state_4 input
	(* query *)
	| '/' -> state_4 input
	| _ -> "error4"
and state_5 input =(* HTTP version *)
	let c = input_char input in
	match c with
	| 'H' -> state_6 input 
	| _ -> "error5"
and state_6 input = 
	let c = input_char input in
	match c with
	| 'T' -> state_7 input
	| _ -> "error6"
and state_7 input =
	let c = input_char input in
	match c with
	| 'T' -> state_8 input
	| _ -> "error7"
				
and state_8 input =
	let c = input_char input in
	match c with
	| 'P' -> state_9 input 
	| _ -> "error8"
and state_9 input =
	let c = input_char input in
	match c with
	| '/' -> state_10 input
	| _ -> "error9"
and state_10 input =
	let c = input_char input in
	match c with
	| '0'..'9' -> state_11 input
	| _ -> "error10"	
and state_11 input =
	let c = input_char input in
	match c with
	| '.' -> state_12 input 
	| _ -> "error11"

and state_12 input =
	let c = input_char input in
	match c with
	| '0'..'9' -> state_13 input (* HTTP version is done *)
	| _ -> "error12"

and state_13 input =
	let c = input_char input in
	match c with
	| '\r' -> state_14 input (* Beginning the first CRLF *)
	| _ -> "error13" 
	
and state_14 input =
	let c = input_char input in
	match c with
	| '\n' -> state_15 input (* end of first line CRLF done *)
	| _ -> "error14" 

and state_15 input =
	let c = input_char input in
	match c with
	| '\r' -> state_16 input (* second CRLF, no headers *)
	| 'a'..'z' | 'A'..'Z' -> state_18 input (* token of the fieldname *)
	| _ -> "error15"

and state_16 input =
	let c = input_char input in
	match c with
	(* end of header *)
	(* | '\n' -> State_17 input *)
	| '\n' -> "ok"
	| _ -> "error16"
and state_18 input =
	let c = input_char input in
	match c with
	| 'a'..'z' | 'A'..'Z' (* tchar *) -> state_18 input (* fieldname *)
	(* end of fieldname *)
	| ':' -> state_19 input 
	| _ -> "error18"

and state_19 input =
	let c = input_char input in
	match c with
	| ' ' -> state_20 input (* OWS *)
	| '\t' -> state_20 input (* OWS *) 
	| 'a'..'z' | 'A'..'Z' | '0'..'9' (*vchar*) -> state_21 input (* beginning fieldvalue *)
	(* end of OWS and no fieldvalue *)
	| '\r' -> state_22 input 
	| _ -> "error19"

and state_20 input =
	let c = input_char input in
	match c with
	| ' ' -> state_20 input (* OWS *)
	| '\t' -> state_20 input (* OWS *)
	| 'a'..'z' | 'A'..'Z' | '0'..'9' (*vchar*) -> state_21 input (* beginning fieldvalue *)
	| _ -> "error20"


and state_21 input =
	let c = input_char input in
	match c with
	| 'a'..'z' | 'A'..'Z' | '0'..'9' (*vchar*) -> state_21 input
	| ' ' -> state_21 input
	| '\t' -> state_21 input
	| '\r' -> state_22 input 
	| _ -> "error21"

and state_22 input =
	let c = input_char input in
	match c with
	| '\n' -> state_23 input
	(* | '\n' -> "ok" *)
	| _ -> "error22"
	

and state_23 input =
	let c = input_char input in
	match c with
	| 'a'..'z' | 'A'..'Z' (* tchar *) -> state_18 input (* another header field *)
	| '\r' -> state_24 input
	| _ -> "error23"

and state_24 input =
	let c = input_char input in
	match c with
	(* | '\n' -> State_17 input *)
	| '\n' -> "OK"
	| _ -> "error24"


let parse input =
	state_1 input 

	
(*



let state_17 input =
	let c = input_char input in
	match c with
	| _ -> Body
	

	

	

	 *)