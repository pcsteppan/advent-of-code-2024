(* Part 1 approach:

1. Parse the input into a list of nodes.
2. Create a graph from the list of nodes, where neighbors are nodes with the same identifier, and which are adjacent.
3. Create a list of regions, where each region is a set of nodes that are connected to each other.
4. For each region, we need to identify two numbers:
   a. The number of nodes in the region (area).
   b. The number of edges in the region (perimeter).
      i. This is basically finding how many nodes have fewer than 4 neighbors, and then taking the difference of 4 * edge_node_count and the sum of neighbors they do have.
5. For each calculated region, we sum their area * perimeter.
*)


type node = { row: int; col: int; id: char };;
module Graph = Map.Make(struct type t = node let compare = compare end);;
module NodeSet = Set.Make(struct type t = node let compare = compare end);;

let _sample =
"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE";;
let _file_input = Day12.Input.file_contents;;

let input = _file_input;;

let parse_input input =
  input
  |> String.split_on_char '\n'
  |> List.mapi (fun i row ->
    row
    |> String.to_seq
    |> List.of_seq
    |> List.mapi (fun j id -> { row = i; col = j; id }));;

let create_graph all_nodes =
  List.fold_left (fun graph node ->
    let get_neighbors all_nodes node =
      let { row; col; id } = node in
      let neighbors = [
        { row = row - 1; col; id };
        { row = row + 1; col; id };
        { row; col = col - 1; id };
        { row; col = col + 1; id };
      ]
      in
      List.filter (fun n -> List.mem n all_nodes) neighbors
    in
    let neighbors = get_neighbors all_nodes node in
    Graph.add node neighbors graph
  ) Graph.empty all_nodes;;

(* takes all nodes from frontier set, and finds all neighbors in the graph, and returns a new set containing all of those nodes *)
let expand_frontier graph frontier =
  NodeSet.to_list frontier
    |> List.map (fun n -> Graph.find n graph)
    |> List.flatten
    |> NodeSet.of_list;;

(* recursively identifies neighboring nodes and returns subset of the original graph, which represent the region *)
let find_region graph node =
  let rec aux region_nodes frontier =
    match (NodeSet.is_empty frontier) with
      | true -> region_nodes
      | false -> 
        (* add nodes in frontier that are valid to the region_nodes set, and recurse down *)
        let valid_nodes = NodeSet.filter (fun n -> n.id = node.id && not (NodeSet.mem n region_nodes)) frontier in
        let new_region_nodes = NodeSet.union valid_nodes region_nodes in
        let new_frontier = expand_frontier graph valid_nodes in
        aux new_region_nodes new_frontier
  in let region_nodes = aux NodeSet.empty NodeSet.(empty |> add node)
  in let subgraph = Graph.filter (fun key_node _ -> NodeSet.mem key_node region_nodes) graph
  in subgraph;;

let find_regions graph =
  let rec aux graph regions =
    match (Graph.is_empty graph) with
      | true -> regions
      | false ->
        let node = Graph.choose graph |> fst in
        let region = find_region graph node in
        let new_graph = Graph.filter (fun key_node _ -> not (Graph.mem key_node region)) graph in
        let new_regions = regions @ [region] in
        aux new_graph new_regions
  in aux graph [];;

let calc_area_of_region region =
  Graph.cardinal region;;

let calc_perimeter_of_region graph =
  Graph.to_list graph 
    |> List.map (fun (_, neighbors) -> 4 - List.length neighbors)
    |> List.fold_left (+) 0;;

let region_value region =
  calc_area_of_region region * calc_perimeter_of_region region;;

let nodes = parse_input input |> List.flatten;;
let graph = create_graph nodes;;

let regions = find_regions graph;;
let result = List.map region_value regions |> List.fold_left (+) 0;;
Printf.printf "part 1: %d\n" result;;

(* Part 2 approach:
   
   For part 2, the only change is how we calculate the perimeter of a region. Rather than counting all edges, we only count contiguous edges.

   To do this, I turned each region into a 'virtual fence', where every edge (missing neighbor) is given an id based on the edge direction.
   Then we do the same 'find regions' logic from part 1 on that new list of nodes, and count those contiguous regions.

   Example:
    Given a region like this:
    
    AAA
    ABA
    BBB

    We turn it into a virtual fence like this for the A subregion:
    .nnn.
    w...e
    w.s.e
    .s.s.

    From which its clear that the perimeter is 6, whereas in part 1 it would have been 12.

*)

let get_direction node1 node2 =
  match (node1.row - node2.row, node1.col - node2.col) with
    | (1, 0) -> 'n'
    | (-1, 0) -> 's'
    | (0, 1) -> 'w'
    | (0, -1) -> 'e'
    | _ -> failwith "invalid direction";;

let get_missing_neighbors graph node =
  let { row; col; id } = node in
  let neighbors = [
    { row = row - 1; col; id };
    { row = row + 1; col; id };
    { row; col = col - 1; id };
    { row; col = col + 1; id };
  ]
  in
  List.filter (fun n -> not (Graph.mem n graph)) neighbors;;

(* for every node in the region, we find the missing neighbors and update their identifier to be their facing direction, then add them to the virtual fence *)
let get_virtual_fence_nodes original_region =
  let rec aux virtual_fence region =
    match (Graph.is_empty region) with
      | true -> virtual_fence
      | false ->
        let node = Graph.choose region |> fst in
        let missing_neighbors = get_missing_neighbors original_region node in
        let missing_neighbors_with_id = List.map (fun n -> { n with id = get_direction node n }) missing_neighbors in
        let new_virtual_fence = List.fold_left (fun acc n -> n :: acc) virtual_fence missing_neighbors_with_id in
        let new_region = Graph.remove node region in
        aux new_virtual_fence new_region
  in aux [] original_region;;

(* 
  in part 2, the value of the perimeter is different. rather than being the value of every edge, its only the value of contiguous straight edges 
  so we turn each region into a virtual fence, and then find the number of distinct 'regions' (edges) in the virtual fence
*)
let calc_perimeter_of_region region = 
  let virtual_fence_nodes = get_virtual_fence_nodes region in
  let virtual_fence = create_graph virtual_fence_nodes in
  let regions = find_regions virtual_fence in
  List.length regions;;

let region_value region =
  calc_area_of_region region * calc_perimeter_of_region region;;

let result = List.map region_value regions |> List.fold_left (+) 0;;
Printf.printf "part 2: %d\n" result;;
