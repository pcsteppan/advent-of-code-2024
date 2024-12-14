type node = {
  row: int;
  col: int;
  num: int;
};;

module NodeMap = Map.Make(struct 
  type t = node 
  let compare = compare 
end);;

module NodeSet = Set.Make(struct 
  type t = node 
  let compare = compare 
end);;

let input = Day10.Samples.file_contents;;

let split_lines s = 
  String.split_on_char '\n' s;;

let split_chars s = 
  List.map (fun line -> 
    String.to_seq line |> List.of_seq
  ) s;;

let nodes: node list = 
  input
  |> split_lines
  |> split_chars
  |> List.mapi (fun i line -> 
      List.mapi (fun j c -> 
        {row = i; col = j; num = Char.code c - Char.code '0'}
      ) line
     )
  |> List.flatten
  |> List.filter (fun n -> n.num >= 0);;

let get_neighbors node_list node = 
  let {row; col; _} = node in
  let neighbors = List.filter (fun n -> 
    let {row = r; col = c; _} = n in
    (r = row && abs (c - col) = 1) || (c = col && abs (r - row) = 1)
  ) node_list in
  List.filter (fun n -> n.num - node.num == 1) neighbors;;

get_neighbors nodes {row = 3; col = 3; num = 0};;

let nodes_and_neighbors = 
  List.map (fun n -> 
    (n, get_neighbors nodes n)
  ) nodes;;

let graph = 
  List.fold_left (fun acc (n, neighbors) -> 
    NodeMap.add n neighbors acc
  ) NodeMap.empty nodes_and_neighbors;;

let rec trace (map, n) =
  match n with
    | {num = 9; _} -> [n]
    | _ -> match NodeMap.find_opt n map with
      | None -> []
      | Some(neighbors) -> match neighbors with 
        | [] -> []
        | l -> List.map (fun elt -> 
          trace (map, elt)
        ) l |> List.flatten;;

let starts = 
  List.filter (fun n -> 
    n.num = 0
  ) nodes;;

let goals_found_for_each_start = 
  List.map (fun n -> 
    trace (graph, n)
  ) starts;;

let count_uniques = fun l -> 
  List.fold_left (fun acc n -> 
    NodeSet.add n acc
  ) NodeSet.empty l 
  |> NodeSet.elements 
  |> List.length;;

let unique_counts = 
  List.map count_uniques goals_found_for_each_start;;

let part1 = 
  List.fold_left (+) 0 unique_counts;;

let part2 = 
  List.fold_left (fun acc l -> 
    List.length l + acc
  ) 0 goals_found_for_each_start;;

Printf.printf "Part 1: %d\n" part1;;
Printf.printf "Part 2: %d\n" part2;;
