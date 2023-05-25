let from_rev_char_list_to_string token =
	String.of_seq ( List.to_seq ( List.rev token ) )

let rec state_1 input token l =
	let c = input_char input in
	match c with
	(* method *)
	| 'a'..'z' | 'A'..'Z' | '-' as x (* tchar *) ->
		 state_1 input (x::token) l
	(* WS *) 
	| ' ' -> 
		let tok = from_rev_char_list_to_string token in
		state_2 input [] (tok::l)
	| _ -> "error1"::[]
and state_2 input token l = (* shall recognize the request-target *)
	let c = input_char input in
	match c with
	(* origin-form *)
	| '/' as x -> 
		state_3 input (x::token) l
	(* asterisk-form *) 
	(* | '*' -> state_x input *)
	| _ -> "error2"::[]
and state_3 input token l = (* absolute-path *)
	let c = input_char input in
	match c with
	(* segment *)
 	| 'a'..'z' | 'A'..'Z' | '0'..'9' (* pchar *) as x -> 
		state_3 input (x::token) l
	(* absolute-path *) 
	| '/' as x -> state_3 input (x::token) l
	(* query *)
	| '?' as x -> state_4 input (x::token) l
	(* request-target is done *)
	| ' ' -> 
		let tok = from_rev_char_list_to_string token in 
		state_5 input [] (tok::l)
	| _ -> "error3"::[]
and state_4 input token l =
	let c = input_char input in
	match c with
	(* query is empty and request_target is done *)
	| ' ' -> 
		let tok = from_rev_char_list_to_string token in 
		state_5 input [] (tok::l)
	(* query *)
	| 'a'..'z' | 'A'..'Z' | '0'..'9' as x (* pchar *) ->  
			state_4 input (x::token) l
	(* query *)
	| '?' as x -> 
		state_4 input (x::token) l
	(* query *)
	| '/' as x -> 
		state_4 input (x::token) l
	| _ -> "error4"::[]
and state_5 input token l =(* HTTP version *)
	let c = input_char input in
	match c with
	| 'H' -> state_6 input token l
	| _ -> "error5"::[]
and state_6 input token l = 
	let c = input_char input in
	match c with
	| 'T' -> state_7 input token l
	| _ -> "error6"::[]
and state_7 input token l=
	let c = input_char input in
	match c with
	| 'T' -> state_8 input token l
	| _ -> "error7"::[]
				
and state_8 input token l =
	let c = input_char input in
	match c with
	| 'P' -> state_9 input token l
	| _ -> "error8"::[]
and state_9 input token l =
	let c = input_char input in
	match c with
	| '/' -> state_10 input token l 
	| _ -> "error9"::[]
and state_10 input token l =
	let c = input_char input in
	match c with
	| '0'..'9' -> state_11 input token l
	| _ -> "error10"::[]
and state_11 input token l =
	let c = input_char input in
	match c with
	| '.' -> state_12 input token l
	| _ -> "error11"::[]

and state_12 input token l =
	let c = input_char input in
	match c with
	| '0'..'9' -> state_13 input token l(* HTTP version is done *)
	| _ -> "error12"::[]

and state_13 input token l =
	let c = input_char input in
	match c with
	| '\r' -> state_14 input token l (* Beginning the first CRLF *)
	| _ -> "error13"::[]
	
and state_14 input token l =
	let c = input_char input in
	match c with
	| '\n' -> state_15 input token l (* end of first line CRLF done *)
	| _ -> "error14"::[]

and state_15 input token l =
	let c = input_char input in
	match c with
	| '\r' -> state_16 input l (* second CRLF, no headers *)
	| 'a'..'z' | 'A'..'Z' as x -> 
		state_18 input (x::token) l (* token of the fieldname *)
	| _ -> "error15"::[]

and state_16 input l =
	let c = input_char input in
	match c with
	(* end of header *)
	(* | '\n' -> State_17 input *)
	| '\n' -> l
	| _ -> "error16"::[]
and state_18 input token l =
	let c = input_char input in
	match c with
	| 'a'..'z' | 'A'..'Z' (* tchar *) as x -> 
		 state_18 input (x::token) l(* fieldname *)
	(* end of fieldname *)
	| ':' -> 
		let tok = from_rev_char_list_to_string token in 
		state_19 input [] (tok::l)
	| _ -> "error18"::[]

and state_19 input token l =
	let c = input_char input in
	match c with
	| ' ' -> state_20 input token l (* OWS *)
	| '\t' -> state_20 input token l (* OWS *) 
	| 'a'..'z' | 'A'..'Z' | '0'..'9' (*vchar*) as x -> 
		state_21 input (x::token) l (* beginning fieldvalue *)
	(* end of OWS and no fieldvalue *)
	| '\r' -> state_22 input token l
	| _ -> "error19"::[]

and state_20 input token l =
	let c = input_char input in
	match c with
	| ' ' -> state_20 input token l (* OWS *)
	| '\t' -> state_20 input token l (* OWS *)
	| 'a'..'z' | 'A'..'Z' | '0'..'9' (*vchar*) as x -> 
		 state_21 input (x::token) l (* beginning fieldvalue *)
	| _ -> "error20"::[]


and state_21 input token l =
	let c = input_char input in
	match c with
	| 'a'..'z' | 'A'..'Z' | '0'..'9' (*vchar*) as x -> 
		state_21 input (x::token) l
	| ' ' -> state_21 input token l
	| '\t' -> state_21 input token l
	| '\r' -> 
		let tok = from_rev_char_list_to_string token in 
		state_22 input [] (tok::l)
	| _ -> "error21"::[]

and state_22 input token l =
	let c = input_char input in
	match c with
	| '\n' -> state_23 input token l
	(* | '\n' -> "ok" *)
	| _ -> "error22"::[]
	

and state_23 input token l =
	let c = input_char input in
	match c with
	| 'a'..'z' | 'A'..'Z' as x (* tchar *) -> 
		 state_18 input (x::token) l (* another header field *)
	| '\r' -> state_24 input l
	| _ -> "error23"::[]

and state_24 input  l =
	let c = input_char input in
	match c with
	(* | '\n' -> State_17 input *)
	| '\n' -> l
	| _ -> "error24"::[]


let parse input =
	state_1 input [] []

	
(*



let state_17 input =
	let c = input_char input in
	match c with
	| _ -> Body
	

	

	

	 *)