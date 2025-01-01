type change_series = (int * int * int * int) * int

module ChangeSeriesSet = Set.Make (struct
  type t = change_series

  let compare = compare
end)

let prune n = n mod 16777216

let next n =
  let a = n * 64 in
  let n = n lxor a in
  let n = prune n in
  let b = n / 32 in
  let n = n lxor b in
  let n = prune n in
  let c = n * 2048 in
  let n = n lxor c in
  prune n

let calc n =
  let rec aux n c = match c with 0 -> n | _ -> aux (next n) (c - 1) in
  aux n 2000

let tuple_lists l1 l2 l3 l4 l4_prices =
  let rec aux l1 l2 l3 l4 l4_prices =
    match (l1, l2, l3, l4, l4_prices) with
    | [], _, _, _, _
    | _, [], _, _, _
    | _, _, [], _, _
    | _, _, _, [], _
    | _, _, _, _, [] ->
        []
    | x1 :: s1, x2 :: s2, x3 :: s3, x4 :: s4, v4 :: vs4 ->
        ((x1, x2, x3, x4), v4) :: aux s1 s2 s3 s4 vs4
  in
  aux l1 l2 l3 l4 l4_prices |> List.sort_uniq (fun (a, _) (b, _) -> compare a b)

let get_progression seed c =
  let rec aux n c =
    match c with 0 -> [ n mod 10 ] | _ -> (n mod 10) :: aux (next n) (c - 1)
  in
  aux seed c

let get_diffs l =
  let l' = List.tl l in
  let len = List.length l in
  let l = List.filteri (fun i _ -> i < len - 1) l in
  List.map2 (fun a b -> b - a) l l'

let get_changes seed c : ChangeSeriesSet.t =
  let progression = get_progression seed c in
  let diffs = get_diffs progression in
  let l1 = diffs in
  let l2 = List.tl l1 in
  let l3 = List.tl l2 in
  let l4 = List.tl l3 in
  let v4 = List.tl @@ List.tl @@ List.tl @@ List.tl progression in
  let changes = tuple_lists l1 l2 l3 l4 v4 in
  List.fold_left
    (fun acc change_series -> ChangeSeriesSet.add change_series acc)
    ChangeSeriesSet.empty changes

let tbl_update tbl (set : ChangeSeriesSet.t) =
  let series_list = ChangeSeriesSet.to_list set in
  let rec aux s =
    match s with
    | [] -> ()
    | (series, v) :: xs -> (
        match Hashtbl.find_opt tbl series with
        | Some count ->
            Hashtbl.replace tbl series (count + v);
            aux xs
        | None ->
            Hashtbl.add tbl series v;
            aux xs)
  in
  aux series_list

let find_most_common_series seeds c =
  let sets_to_count = List.map (fun seed -> get_changes seed c) seeds in
  let tbl = Hashtbl.create 10000 in
  let rec aux sets =
    match sets with
    | [] -> ()
    | set :: set_s ->
        tbl_update tbl set;
        aux set_s
  in
  let () = aux sets_to_count in
  let frequencies =
    Hashtbl.to_seq tbl |> List.of_seq
    |> List.sort (fun (_, c) (_, c2) -> compare c c2)
    |> List.rev
  in
  List.hd frequencies

(* Part 1 *)
let input_list =
  Day22.Input.file_contents |> String.split_on_char '\n'
  |> List.map int_of_string

let results = List.map calc input_list
let sum = List.fold_left ( + ) 0 results;;

print_endline "Part 1";;
Printf.printf "Sum: %d\n" sum;;

(* Part 2 *)
print_endline "Part 2"

let result = find_most_common_series input_list 2000
let (a, b, c, d), count = result

let () =
  Printf.printf "Most common series: (%d, %d, %d, %d), Count: %d\n" a b c d
    count
