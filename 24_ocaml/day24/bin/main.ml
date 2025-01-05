type gate = AND | OR | XOR
type logic_unit = { a : string; b : string; gate : gate; o : string }

let parse_constants input =
  let relevant_lines =
    List.filter (fun line -> String.contains line ':')
    @@ String.split_on_char '\n' input
  in
  List.map
    (fun line ->
      let parts = String.split_on_char ':' line in
      ( String.trim @@ List.hd parts,
        int_of_string @@ String.trim @@ List.nth parts 1 ))
    relevant_lines

let parse_logic_units input =
  let relevant_lines =
    List.filter (fun line -> String.contains line '-')
    @@ String.split_on_char '\n' input
  in
  List.map
    (fun line ->
      let parts = String.split_on_char ' ' line in
      let a = List.nth parts 0 in
      let b = List.nth parts 2 in
      let gate =
        match List.nth parts 1 with
        | "AND" -> AND
        | "OR" -> OR
        | "XOR" -> XOR
        | _ -> failwith "Invalid gate"
      in
      let o = List.nth parts 4 in
      { a; b; gate; o })
    relevant_lines

let get_unit logic_units name = List.find (fun { o; _ } -> o = name) logic_units

let compute unit a b =
  match unit with AND -> a land b | OR -> a lor b | XOR -> a lxor b

let find_z_values constants logic_units =
  let cache = Hashtbl.create 1000 in
  List.iter (fun (name, state) -> Hashtbl.add cache name state) constants;
  let rec aux name =
    match Hashtbl.find_opt cache name with
    | Some state -> state
    | None ->
        let unit = get_unit logic_units name in
        let state = compute unit.gate (aux unit.a) (aux unit.b) in
        Hashtbl.add cache name state;
        state
  in
  let z_units =
    List.filter_map
      (fun { o; _ } ->
        if String.starts_with ~prefix:"z" o then Some o else None)
      logic_units
    |> List.sort compare |> List.rev
  in
  List.map aux z_units

let bin_to_decimal (bin_list : int list) =
  let bin_list = List.rev bin_list in
  let rec aux bin_list =
    match bin_list with
    | [] -> 0
    | [ x ] -> x
    | x :: s -> Int.shift_left (aux s) 1 + x
  in
  aux bin_list

let solve input =
  let constants = parse_constants input in
  let logic_units = parse_logic_units input in
  let z_values = find_z_values constants logic_units in
  let answer = bin_to_decimal z_values in
  print_endline ("part 1: " ^ string_of_int answer)

let input = Day24.Input.file_contents;;

solve input
