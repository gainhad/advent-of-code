open Base
open Stdio

let p1_calculate_fuel mass = mass / 3 - 2

let rec p2_calculate_fuel accum mass =
  match mass with
  | 0 -> accum
  | _ -> 
      let fuel = mass / 3 - 2 in
      let rounded_fuel = if fuel > 0 then fuel else 0 in
      let new_total = accum + rounded_fuel in
      p2_calculate_fuel new_total rounded_fuel

let p1 lines =
  lines
  |> List.fold ~init:0 ~f:(fun total line ->
      let mass = Int.of_string line in
      total + p1_calculate_fuel mass)

let p2 lines =
  lines
  |> List.fold ~init:0 ~f:(fun total line ->
      let mass = Int.of_string line in
      total + p2_calculate_fuel 0 mass)

let solve () = 
  let lines = In_channel.read_lines "inputs/1.txt" in
  printf "Part One Total: %d\n" (p1 lines);
  printf "Part Two Total: %d\n" (p2 lines)
