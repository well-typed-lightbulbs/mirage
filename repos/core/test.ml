open Lwt

let main () =
  Printf.printf "foobar\n%!";
  lwt () = OS.Time.sleep 5. in
  Printf.printf "fooba2r\n%!";
  return ()

let _ =  OS.Main.run (main ())
