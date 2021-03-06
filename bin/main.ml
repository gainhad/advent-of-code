open Base
open Stdio
open Aoc

let solve num =
  match num with
    | 1 -> P1.solve ()
    | 2 -> P2.solve ()
    | 3 -> P3.solve ()
    | _ -> printf "Not solved yet, sorry :-)\n"

let () =
  let args = Sys.get_argv () in
  let p_number = Int.of_string args.(1) in
  solve p_number
