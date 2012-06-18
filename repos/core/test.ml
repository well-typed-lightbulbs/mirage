open Lwt

let main () =
  Printf.printf "foobar\n%!";
  return ()

let _ =  OS.Main.run (main ())
