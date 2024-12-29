module NaiveMinimumPriorityQueue = struct
  type 'a queue = (int * 'a) list

  let empty = []

  let rec enqueue x q =
    match q with
    | [] -> [ x ]
    | x' :: s' -> if x < x' then x :: q else x' :: enqueue x s'

  let peek = function [] -> None | x :: _ -> Some x
  let dequeue = function [] -> failwith "Empty queue" | _ :: s -> s
end

module PQ = NaiveMinimumPriorityQueue

type direction = N | E | S | W

let index_range o = List.init o (fun i -> i)

let index_range_2d height width =
  index_range height
  |> List.map (fun row -> index_range width |> List.map (fun col -> (row, col)))
  |> List.flatten

let find_direction_of (x, y) (x', y') =
  match (x' - x, y' - y) with
  | 0, 1 -> E
  | 0, -1 -> W
  | 1, 0 -> S
  | -1, 0 -> N
  | _, _ -> failwith "Invalid arguments"

let rec get_directions x s =
  match s with
  | [] -> []
  | x' :: s -> find_direction_of x x' :: get_directions x s

let get_neighbors (row, col) (height, width) =
  let in_bounds (row, col) =
    row >= 0 && row < height && col >= 0 && col < width
  in
  let possibles =
    [ (row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1) ]
  in
  List.filter in_bounds possibles

let get_neighbors_with_directions n dim =
  let neighbors = get_neighbors n dim in
  List.map
    (fun (row, col) -> (row, col, find_direction_of n (row, col)))
    neighbors

let direction_to_string = function E -> "E" | W -> "W" | S -> "S" | N -> "N"

let print_node_with_cost (c, (a, b, d)) =
  Printf.printf "n: (%d %d %s) c: %d\n" a b (direction_to_string d) c

let get_direction_with_complement = function
  | E -> [ E; W ]
  | W -> [ E; W ]
  | N -> [ N; S ]
  | S -> [ N; S ]

let get_directions_with_complements directions =
  List.map (fun d -> get_direction_with_complement d) directions
  |> List.flatten
  |> List.sort_uniq (fun d1 d2 -> compare d1 d2)

(* nodes in this graph look like (int * int * direction ) where char represents direction
   edges are (node * node * int) where int represents the weight of the edge
   this is not a general purpose graph! *)
module Graph = struct
  type 'a edge = 'a * 'a * int
  type 'a graph = { nodes : 'a list; edges : 'a edge list }

  let empty = { nodes = []; edges = [] }
  let create nodes edges = { nodes; edges }

  let get_edges node g =
    let is_match (a, _, _) = a = node in
    List.filter is_match g.edges |> List.map (fun (_, b, w) -> (b, w))

  let get_all_edges_from_pos ((a, b) : int * int) g =
    List.map (fun d -> get_edges (a, b, d) g) [ N; E; S; W ] |> List.flatten

  let add_edge edge g = { g with edges = edge :: g.edges }

  let rec add_edges edges g =
    match edges with [] -> g | x :: s -> add_edges s (add_edge x g)

  (* takes an intint and list of directions. nodes are made for each direction with the intint. 
     those nodes are all linked together. *)
  let add_node_interdirectionality (intint : int * int)
      (directions : direction list) (g : 'a graph) : 'a graph =
    (* List.iter (fun d -> print_endline (direction_to_string d)) directions; *)
    let row, col = intint in
    let nodes_to_add = List.map (fun dir -> (row, col, dir)) directions in
    let generate_bi_edges x s =
      List.map (fun n -> [ (n, x, 1000); (x, n, 1000) ]) s |> List.flatten
    in
    let rec aux nodes g =
      (* link first node with each remaining node with cost 1000 *)
      match nodes with
      | [] -> g
      | x :: s -> aux s (add_edges (generate_bi_edges x s) g)
    in
    aux nodes_to_add g

  let add_directional_edge a b g =
    let a_to_b = find_direction_of a b in
    let ax, ay = a in
    let bx, by = b in
    let to_pair = ((ax, ay, a_to_b), (bx, by, a_to_b), 1) in
    add_edge to_pair g

  let rec add_directional_edges a s g =
    match s with
    | [] -> g
    | x :: s -> add_directional_edges a s (add_directional_edge a x g)

  let from (data : bool list list) =
    let height = List.length data in
    let width = List.length (List.hd data) in
    let indices = index_range_2d height width in
    let is_node_at_index (row, col) = List.nth (List.nth data row) col = true in
    let nodes = List.filter is_node_at_index indices in
    let get_local_neighbors node =
      get_neighbors node (height, width)
      |> List.filter (fun n -> List.mem n nodes)
    in
    (* for every node, find neighbors, find directions, add interdirectionality, add directional edges *)
    let g =
      List.fold_left
        (fun acc curr ->
          let ns = get_local_neighbors curr in
          let directions =
            get_directions curr ns |> get_directions_with_complements
          in
          let g = add_directional_edges curr ns acc in
          add_node_interdirectionality curr directions g)
        empty nodes
    in
    g

  (* returns graph, start, and goal *)
  let parse (input : string) : 'a graph * 'a * (int * int) =
    let data =
      input |> String.split_on_char '\n'
      |> List.map (fun s ->
             List.init (String.length s) (fun i ->
                 s.[i] = '.' || s.[i] = 'S' || s.[i] = 'E'))
    in
    let start = ref (0, 0, E) in
    let goal = ref (0, 0) in
    let update_start_and_goal row col c =
      if c = 'S' then start := (row, col, E)
      else if c = 'E' then goal := (row, col)
    in
    List.iteri
      (fun row line ->
        List.iteri
          (fun col c -> update_start_and_goal row col c)
          (String.to_seq line |> List.of_seq))
      (String.split_on_char '\n' input);
    let g = from data in
    (g, !start, !goal)

  (* returns 0 for unfound goal (bug).
     returns the cost of the cheapest path and also a 'trace', which is a graph used to determine the best paths found from start to goal (it's possible there are multiple best paths)
  *)
  let dijkstra (start : 'a) (goal : int * int) (g : 'a graph) =
    let visited = ref (Hashtbl.create 10000) in
    let trace = ref empty in
    let pq = ref PQ.(empty |> enqueue (0, start)) in
    let total_cost = ref 0 in
    while !pq <> PQ.empty do
      let curr = PQ.peek !pq |> Option.get in
      pq := PQ.dequeue !pq;
      let curr_cost, curr_node = curr in
      let cx, cy, _ = curr_node in
      if (cx, cy) = goal then (
        total_cost := curr_cost;
        pq := PQ.empty)
      else if not (Hashtbl.mem !visited curr_node) then (
        let neighbors = get_edges curr_node g in
        let next_with_costs =
          List.map (fun (node, weight) -> (curr_cost + weight, node)) neighbors
        in
        let updated_queue =
          List.fold_left
            (fun acc curr ->
              let cost, node = curr in
              let edge = (node, curr_node, cost) in
              trace := add_edge edge !trace;
              PQ.enqueue curr acc)
            !pq next_with_costs
        in
        pq := updated_queue;

        Hashtbl.add !visited curr_node true)
    done;
    (!total_cost, trace)

  (* takes a list like: [((0, 1, W), 2001); ((0, 1, N), 2001); ((0, 1, E), 1001)]
   and returns only the cheapest edges from the list *)
  let cheapest_edges edges =
    let min =
      List.fold_left
        (fun acc (_, c) -> if c < acc then c else acc)
        Int.max_int edges
    in
    List.filter (fun e -> snd e = min) edges

  let collect_all_from_end_to_start g start goal =
    let q = ref (get_all_edges_from_pos goal g) in
    let collection = ref [ goal ] in
    while !q <> [] do
      match !q with
      | [] -> ()
      | (x, _) :: s ->
          q := s;
          let row, col, _ = x in
          collection := (row, col) :: !collection;
          if x <> start then
            let nexts = get_edges x g |> cheapest_edges in
            let new_q = List.fold_left (fun acc curr -> curr :: acc) !q nexts in
            q := new_q
    done;
    !collection |> List.sort_uniq (fun a b -> compare a b)
end

let _min_sample = "SE"
let _min_sample2 = "S.\n.E"
let _min_sample3 = "S.#\n..E"

let _sample =
  "###############\n\
   #.......#....E#\n\
   #.#.###.#.###.#\n\
   #.....#.#...#.#\n\
   #.###.#####.#.#\n\
   #.#.#.......#.#\n\
   #.#.#####.###.#\n\
   #...........#.#\n\
   ###.#.#####.#.#\n\
   #...#.....#.#.#\n\
   #.#.#.###.#.#.#\n\
   #.....#...#.#.#\n\
   #.###.#.#.#.#.#\n\
   #S..#.....#...#\n\
   ###############"

let input = Day16.Input.file_contents
let g, start, goal = Graph.parse input
let min_dist, trace = Graph.dijkstra start goal g
let count = Graph.collect_all_from_end_to_start !trace start goal |> List.length
;;

Printf.printf "(part 1) min dist from start to end: %d\n" min_dist;
Printf.printf "(part 2) count of nodes in best path(s): %d\n" count
