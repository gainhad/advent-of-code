open Stdio

type point = 
  { x : int;
    y : int;
  }

type segment =
  { p1 : point;
    p2 : point;
  }


let solve () = printf "solving 3...\n"
