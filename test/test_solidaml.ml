open Solidaml

let rec   print_list l = 
  match l with 
  | [] -> Printf.printf "\n"
  | x::xs -> Printf.printf "%s\n" x; print_list xs

let () = 
  let filename = "test" in 
  let req = open_in filename in 
  let res = Rfc7230.parse req in 
  (* let () = Printf.printf "%s\n" res in  *)
  print_list (List.rev res);
  close_in req