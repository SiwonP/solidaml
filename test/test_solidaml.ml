open Solidaml

let () = 
  let filename = "test" in 
  let req = open_in filename in 
  let res = Rfc7230.parse req in 
  let () = Printf.printf "%s\n" res in 
  close_in req