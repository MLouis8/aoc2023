let read_file file =
  let ic = open_in file in
  let rec aux l =
    match input_line ic with
    | line                  -> aux (line :: l)
    | exception End_of_file -> close_in ic; List.rev l
  in
  aux []

let to_int i = i - 48

(* let calibration1 text =
  List.fold_left (
    fun acc line ->
      let n1, n2 = String.fold_left (
        fun ns c ->
          let code = int_of_char c in
          if code <= 57 && code >= 48 then
          if snd(fst ns) then (fst ns, to_int code)
          else ((to_int code, true), to_int code)
    else
      ns
    ) ((0, false), 0) line in
    acc + fst n1 * 10 + n2
  ) 0 text *)

let rec match_digit = function
  | "zero"  -> 0
  | "one"   -> 1
  | "two"   -> 2
  | "three" -> 3
  | "four"  -> 4
  | "five"  -> 5
  | "six"   -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine"  -> 9
  | other ->
    let l = String.length other in
    if l < 3 || l = 0
    then
      -1
    else
      let short = match_digit (String.sub other 0 (l-1) ) in
      if short = -1 then
        match_digit (String.sub other 1 (l-1) )
      else
        short

let string_add s1 c =
  let l = String.length s1 in
  if c = '$' && l > 0 then
    String.sub s1 1 (l-1)
  else
    let s2 = s1 ^ String.make 1 c in
    if String.length s1 > 4
    then String.sub s2 1 5
    else s2

let update_digit_spelled f ns d =
  if d <> -1 then
    (* let _ = Printf.printf "yup %d\n" d in *)
    if !f then (fst ns, d)
    else let _ = f := true in (d, d)
  else
    ns


let calibration2 (text: string list): int =
  List.fold_left (
    fun acc line ->
      let spelled = ref "" in
      let flag = ref false in 
      let n1, n2 = String.fold_left (
        fun ns c ->
          let _ = Printf.printf "%s\n" !spelled in
          let code = int_of_char c in
          if code <= 57 && code >= 48 then
            let _ = spelled := "" in
            if !flag then
              (fst ns, to_int code)
            else
              let _ = flag := true in
              (to_int code, to_int code)     
          else
            let _ = spelled := string_add !spelled c in
            let digit = match_digit !spelled in
            update_digit_spelled flag ns digit
      ) (0, 0) (line^"$$$") in
      acc + n1 * 10 + n2
  ) 0 text

let () =
  let text = read_file "1./calibration.txt" in
  print_int (calibration2 text);