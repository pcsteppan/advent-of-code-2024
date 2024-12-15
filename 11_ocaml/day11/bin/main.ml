(* let input = [125; 17];; *)
let input = [
  1; 24596; 0; 740994; 60; 803; 8918; 9405859
];;

module IntMap = Map.Make(Int);;

let digits_of_int n =
  let rec aux acc n =
    if n < 10 then n :: acc
    else aux (n mod 10 :: acc) (n / 10)
  in
  aux [] n;;

let digit_list_to_int lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (10 * acc + hd) tl
  in
  aux 0 lst;;

(* takes first half and second half and return as pair of two ints *)
let hashtable = Hashtbl.create 100000;;
let split_int (elt: int) : int * int =
  if Hashtbl.mem hashtable elt then
    Hashtbl.find hashtable elt
  else
    let digits = digits_of_int elt in
    let half = List.length digits / 2 in
    let digits_with_index = List.mapi (fun i x -> (i, x)) digits in
    let first_half, second_half = List.partition (fun (i, _) -> i < half) digits_with_index in
    let first_half = List.map snd first_half in
    let second_half = List.map snd second_half in
    let first_half = digit_list_to_int first_half in 
    let second_half = digit_list_to_int second_half in
    let result = (first_half, second_half) in
    Hashtbl.add hashtable elt result;
    result;;

let has_even_digits n =
  if n = 0 then true
  else if n < 10 then false
  else if n < 100 then true
  else if n < 1000 then false
  else if n < 10000 then true
  else if n < 100000 then false
  else if n < 1000000 then true
  else if n < 10000000 then false
  else if n < 100000000 then true
  else if n < 1000000000 then false
  else if n < 10000000000 then true
  else if n < 100000000000 then false
  else if n < 1000000000000 then true
  else if n < 10000000000000 then false
  else if n < 100000000000000 then true
  else if n < 1000000000000000 then false
  else if n < 10000000000000000 then true
  else if n < 100000000000000000 then false
  else if n < 1000000000000000000 then true
  else let length = String.length (string_of_int n) in length mod 2 = 0;;

let update_map stone count new_map_acc =
  IntMap.update stone (fun key -> 
    match key with 
    | Some(c) -> Some (c + count) 
    | None -> Some count) 
  new_map_acc;;

let blink init_map generations =
  let rec aux map gens =
    match gens == 0 with
    | true -> map
    | false ->
      let new_map = IntMap.fold (fun stone count new_map_acc ->
        match stone with
        | 0 -> 
          update_map 1 count new_map_acc
        | n when has_even_digits n -> 
          let (first_half, second_half) = split_int stone in
          let new_map_acc = update_map first_half count new_map_acc in
          update_map second_half count new_map_acc
        | n -> 
          update_map (n * 2024) count new_map_acc
        ) map IntMap.empty
    in aux new_map (gens - 1)
  in aux init_map generations;;


let input_map = List.fold_left (fun acc elt -> IntMap.add elt 1 acc) IntMap.empty input;;
let calculate_result input_map generations =
  let stone_counts = blink input_map generations in
  IntMap.fold (fun _stone count acc -> acc + count) stone_counts 0;;

let result1 = calculate_result input_map 25;;
let result2 = calculate_result input_map 75;;

Printf.printf "result1: %d\nresult2: %d\n" result1 result2;;