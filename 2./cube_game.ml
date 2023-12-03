open Aoc2023

let content = [|12; 13; 14|]

let match_color col n = match col with
  | "red"   -> n <= content.(0)
  | "blue"  -> n <= content.(1)
  | "green" -> n <= content.(2)
  | other -> failwith (""^other^": color string not valid, must be red or blue or green")

let get_amount s =
  let code = int_of_char s.[2] in
  if code <= 57 && code >= 48 then
    File.to_int s.[1] * 10 + File.to_int s.[2], 4
  else
    File.to_int s.[1], 3

let is_correct b revelation =
  let color_batch = String.split_on_char ',' revelation in
  b && List.fold_left (
    fun acc color ->
      let amount, size = get_amount color in
      acc && match_color (String.sub color size (String.length color - size)) amount
  ) true color_batch 

let cube_game1 text =
  List.fold_left (
    fun acc line ->
      let game = match String.split_on_char ':' line with
       | [] -> assert false
       | _::t -> match t with
          | [] -> assert false
          | h2::_ -> h2
      in
      let revelations = String.split_on_char ';' game in
      let correct = List.fold_left is_correct true revelations in 
      if correct then
        (fst acc, snd acc+1)
      else
        let _ = Printf.printf "%d %d\n" (fst acc) (snd acc) in
        (fst acc + snd acc, snd acc+1)
  ) (0,1) text

let () =
  let text = File.read_file "2./cube_data.txt"
  in print_int (fst (cube_game1 text))
