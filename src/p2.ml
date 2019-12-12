open Base
open Stdio

type instruction =
  | Add of int * int * int 
  | Multiply of int * int * int
  | Exit
  | Invalid

let create_instruction list pointer =
  let ints = List.take (List.drop list pointer) 4 in
  match ints with
  | 1::x::y::z::_ -> Add (x, y, z)
  | 2::x::y::z::_ -> Multiply (x, y, z)
  | 99::_ -> Exit
  | _ -> Invalid

let replace pointer n list  =
  List.mapi ~f:(fun i x -> if i = pointer then n else x) list

let run_add x y z list= 
  let new_num = (List.nth_exn list x) + (List.nth_exn list y) in
  replace z new_num list

let run_multiply x y z list =
  let new_num = (List.nth_exn list x) * (List.nth_exn list y) in
  replace z new_num list

let rec eval_program program pointer =
  let instruction = create_instruction program pointer in
  let new_pointer = pointer + 4 in
  match instruction with
  | Add (x,y,z) -> eval_program (run_add x y z program) new_pointer
  | Multiply (x,y,z)-> eval_program (run_multiply x y z program) new_pointer
  | Exit -> program
  | Invalid -> program

let rec find_output program (noun,verb) =
  let updated_program = replace 1 noun program |> replace 2 verb in
  let result = List.hd_exn (eval_program updated_program 0) in
  match result with
  | 19690720 -> (noun, verb)
  | _ ->
      if verb < 99 then find_output program (noun, verb + 1) 
      else find_output program (noun + 1, 0)

let make_program string =
  String.split ~on:',' string
  |> List.map ~f:(fun x -> String.strip x |> Int.of_string)

let make_part_one_program list = replace 1 12 list |> replace 2 2

let solve () =
  let program = make_program (In_channel.read_all "inputs/2.txt") in
  let part_one_program = make_part_one_program program in
  printf "Part 1: %d\n" (List.hd_exn (eval_program part_one_program 0));
  let (n, v) = find_output program (0,0) in
  printf "Part 2: noun:%d, verb:%d\n" n v
