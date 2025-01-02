let connections input =
  input |> String.split_on_char '\n'
  |> List.map (fun x -> String.split_on_char '-' x)
  |> List.map (fun x -> (List.hd x, List.nth x 1))

let graph connections =
  let g = Hashtbl.create 100 in
  List.iter
    (fun (a, b) ->
      let a_edges =
        match Hashtbl.find_opt g a with Some x -> x | None -> []
      in
      let b_edges =
        match Hashtbl.find_opt g b with Some x -> x | None -> []
      in
      Hashtbl.replace g a (b :: a_edges);
      Hashtbl.replace g b (a :: b_edges))
    connections;
  g

(* does a graph search that returns all triangles from a specific node *)
let get_triangles origin_node g =
  let visited = Hashtbl.create 100 in
  let rec aux i n g : string list list =
    if i > 2 then if n = origin_node then [ [ n ] ] else []
    else if i = 2 && Hashtbl.mem visited n then []
    else (
      if i = 1 then Hashtbl.add visited n true;
      let neighbors = Hashtbl.find g n in
      let nexts =
        List.map (fun x -> aux (i + 1) x g) neighbors
        |> List.flatten
        |> List.filter (fun x -> List.length x > 0)
      in
      if i > 0 then List.map (fun x -> n :: x) nexts else nexts)
  in
  aux 0 origin_node g |> List.map (fun n -> List.sort compare n)

let get_all_triangles g =
  let nodes =
    Hashtbl.fold (fun k _ acc -> k :: acc) g []
    |> List.filter (fun n -> String.starts_with ~prefix:"t" n)
  in
  List.map (fun n -> get_triangles n g) nodes
  |> List.flatten |> List.sort_uniq compare

module Set = Set.Make (String)

let get_largest_subgraph_from n g =
  let overall_visited = ref Set.empty in
  let n_set = n :: Hashtbl.find g n |> Set.of_list in
  let rec aux g visited acc n =
    if Set.mem n visited || Set.subset acc !overall_visited then Set.empty
    else
      let neighbors = Hashtbl.find g n in
      let new_visited = Set.add n visited in
      let new_acc = Set.inter (n :: neighbors |> Set.of_list) acc in
      if Set.equal new_acc new_visited then (
        overall_visited := Set.union !overall_visited new_acc;
        new_acc)
      else
        List.fold_left
          (fun acc neighbor ->
            let next = aux g new_visited new_acc neighbor in
            if Set.cardinal next > Set.cardinal acc then next else acc)
          Set.empty neighbors
  in
  aux g Set.empty n_set n

let find_largest_subgraph g =
  let nodes = Hashtbl.fold (fun k _ acc -> k :: acc) g [] in
  let rec aux nodes =
    match nodes with
    | [] -> []
    | x :: s ->
        print_endline ("Checking node: " ^ x);
        let next = get_largest_subgraph_from x g in
        let next_nodes = s |> List.filter (fun n -> not (Set.mem n next)) in
        next :: aux next_nodes
  in
  aux nodes
  |> List.sort_uniq (fun a b -> compare (Set.cardinal b) (Set.cardinal a))
  |> List.hd

let _print_graph g =
  Hashtbl.iter (fun k v -> Printf.printf "%s: %s\n" k (String.concat ", " v)) g

let input = Day23.Input.file_contents
let g = connections input |> graph;;

get_all_triangles g |> List.length |> string_of_int |> print_endline;;

find_largest_subgraph g |> Set.to_list |> List.sort compare |> String.concat ","
|> print_endline
