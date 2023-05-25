open Unix

type token_type =
  | TOKEN of string 
  | CRLF 


type start_line = { meth:string; path:string  }

let rec scan input tokens t v state =
  let c = input_char input in
  match c with 
  | 'a'..'z'
  | 'A'..'Z'
  | '0'..'9' -> scan input 
  | ' ' -> scan 

let scan_token c = function 
  | 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
  | _ -> false 

let scan_ws c = function 
  | ' ' -> true
  | _ -> false 


let parse_req req = 
  match String.split_on_char ' ' req with 
  | meth :: path :: _ -> {meth=meth; path=path }
  | _ -> {meth="None"; path = "None"} 

let rec sequence_req_aux in_channel l acc = 
  match acc with 
  | 2 -> List.rev l
  | 1 | 0 -> let c = input_char in_channel in 
    (match c with 
    | '\r' as x -> match_end_of_headers in_channel (x::l) acc
    | _ as x -> sequence_req_aux in_channel (x::l) 0)
  | _ -> []
and match_end_of_headers in_channel l acc = 
  let c = input_char in_channel in
  match c with 
  | '\n' as x -> sequence_req_aux in_channel (x::l) (acc+1)
  | _ as x -> sequence_req_aux in_channel (x::l) 0

let sequence_req in_channel = 
  sequence_req_aux in_channel [] 0


let handle_request client_socket =
  let input = in_channel_of_descr client_socket in  
  (* let request_line = input_line input in *)
  let l = sequence_req input in 
  let req = String.of_seq (List.to_seq l) in 
  let first_line = parse_req req in
  Printf.printf "%s\n%s" first_line.meth first_line.path
  (* Printf.printf "%s\n" request_line *)


let rec accept_connections server_socket =
  let (client_socket, _) = accept server_socket in
  match fork () with
  | 0 ->  (* Child process *)
    close server_socket;
    handle_request client_socket;
    exit 0
  | _ ->  (* Parent process *)
    (* close client_socket; *)
    accept_connections server_socket

let main () =
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  let () = setsockopt server_socket SO_REUSEADDR true in
  let server_address = ADDR_INET (inet_addr_loopback, 8080) in
  let () = bind server_socket server_address in
  let () = listen server_socket 10 in
  
  accept_connections server_socket

let () = main ()
