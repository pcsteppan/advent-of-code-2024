type eq = {a: float; b: float; x: float}

let _sample =
"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279";;

let input = Day13.Input.file_contents;;

let create_equations list =
  match list with
  | [a; b; c; d; e; f] -> 
    (
      {a = float_of_int a; b = float_of_int c; x = float_of_int e},
      {a = float_of_int b; b = float_of_int d; x = float_of_int f}
    )
  | _ -> failwith "Invalid input";;

(* split_nth returns a list of lists of size n each *)
let rec chunk_list lst n =
  match lst with
  | [] -> []
  | _ -> 
      let rec take_n lst n acc =
        match lst, n with
        | _, 0 -> (List.rev acc, lst)  (* Return accumulated sublist and the remainder of the list *)
        | [], _ -> (List.rev acc, [])  (* In case the list is shorter than n, return the list as it is *)
        | x :: xs, _ -> take_n xs (n - 1) (x :: acc)
      in
      let chunk, remainder = take_n lst n [] in
      chunk :: chunk_list remainder n  (* Recursively process the remainder of the list *)

let parse_input input =
  let clean = Str.global_replace (Str.regexp "[^0-9 ]") "" input in
  let split = Str.split (Str.regexp " ") clean in
  List.filter (fun x -> x <> "") split
  |> (fun x -> chunk_list x 6)
  |> List.map (fun x -> List.map int_of_string x)
  |> List.map create_equations;;

let equations: (eq * eq) list = parse_input input;;

let solve_equations eq1 eq2 =
  let eq1_in_terms_of_a = {a = 1.; b = eq1.b /. eq1.a; x = eq1.x /. eq1.a} in
  let eq2_in_terms_of_a = {a = 1.; b = eq2.b /. eq2.a; x = eq2.x /. eq2.a} in
  if eq1_in_terms_of_a.b = eq2_in_terms_of_a.b then
    None
  else
    let a = (eq2_in_terms_of_a.x -. eq1_in_terms_of_a.x) /. (eq1_in_terms_of_a.b -. eq2_in_terms_of_a.b) in
    let b = eq1_in_terms_of_a.b *. a +. eq1_in_terms_of_a.x in
    Printf.printf "a: %f, b: %f\n" a b;
    Some (Float.round (-.a) |> Int.of_float, Float.round b |> Int.of_float);;

let tokens (a, b) = b * 3 + a;;

type int_eq = {a: int; b: int; x: int};;
let int_eq (f_eq: eq) = {a = f_eq.a |> Int.of_float; b = f_eq.b |> Int.of_float; x = f_eq.x |> Int.of_float};;

let is_valid_solution (a, b) (eq1, eq2) =
  let ieq1 = int_eq eq1 in
  let ieq2 = int_eq eq2 in
  (* log everything out *)
  Printf.printf "a: %d, b: %d\n" a b;
  Printf.printf "eq1: a: %d, b: %d, x: %d\n" ieq1.a ieq1.b ieq1.x;
  Printf.printf "eq2: a: %d, b: %d, x: %d\n" ieq2.a ieq2.b ieq2.x;
  (ieq1.a * b + ieq1.b * a = ieq1.x) && (ieq2.a * b + ieq2.b * a = ieq2.x);;

let token_sum = List.fold_left (fun acc (eq1, eq2) ->
  match (solve_equations eq1 eq2) with
  | None -> acc
  | Some (a, b) -> if is_valid_solution (a, b) (eq1, eq2) then acc + tokens (a, b) else acc
) 0 equations;;

Printf.printf "Part 1 sum of tokens: %d\n" token_sum;;

(* Part 2 *)
(* Update equations to have xs with +10000000000000 *)

let add_offset ((eq1, eq2) : eq * eq) offset = ({eq1 with x = eq1.x +. offset}, {eq2 with x = eq2.x +. offset});;
let new_equations = List.map (fun (eq1, eq2) -> add_offset (eq1, eq2) 10000000000000.) equations;;

let token_sum = List.fold_left (fun acc (eq1, eq2) ->
  match (solve_equations eq1 eq2) with
  | None -> acc
  | Some (a, b) -> if is_valid_solution (a, b) (eq1, eq2) then acc + tokens (a, b) else acc
) 0 new_equations;;

Printf.printf "Part 2 sum of tokens: %d\n" token_sum;;
