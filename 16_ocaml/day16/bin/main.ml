let sub (ax, ay) (bx, by) = (ax - bx, ay - by)

let north = (-1, 0)
let east = (0, 1)
let south = (1, 0)
let west = (0, -1)

let cost a b dir = if sub b a = dir then 1 else 1001;;

module Graph = Map.Make(struct type t = int * int let compare = compare end)

let input = [
  [1; 1; 0; 1;];
  [1; 0; 0; 1;];
  [1; 0; 0; 1;];
  [1; 1; 1; 1;];
]

let create_graph input =
  let graph = ref Graph.empty in
  let width = List.length (List.hd input) in
  let height = List.length input in
  for row = 0 to height do
    for col = 0 to width do
      let neighbors = [
        (row - 1, col    );
        (row + 1, col    );
        (row,     col - 1);
        (row,     col + 1);
      ]
      in
      let neighbors = List.filter (fun (row, col) ->
        row >= 0 && row < width && col >= 0 && col < height && List.nth (List.nth input row) col = 1
      ) neighbors
      in
      graph := Graph.add (row, col) neighbors !graph
    done
  done;
  !graph;;

let print_graph graph =
  Graph.iter (fun (row, col) neighbors ->
    Printf.printf "Node: (%d, %d)\n" row col;
    List.iter (fun (row, col) ->
      Printf.printf "  Neighbor: (%d, %d)\n" row col;
    ) neighbors;
  ) graph;;

type problem = {
  graph: (int * int) list Graph.t;
  start: int * int;
  goal: int * int;
}

let problem = {
  graph = create_graph input;
  start = (0, 0);
  goal = (3, 3);
};;

module NodeSet = Set.Make(struct type t = int * int let compare = compare end);;

let find_min_cost_traversal problem : int option =
  let rec aux (curr: int * int) (curr_dir: int * int) (visited: NodeSet.t) : int option =

    if curr = problem.goal 
      then Some 0
      else (
        let new_visited = NodeSet.add curr visited
        in
        let all_next_nodes: (int * int) list = Graph.find curr problem.graph
        in
        let unvisited_nodes = List.filter (fun n -> NodeSet.mem n new_visited) all_next_nodes
        in
        if List.length unvisited_nodes = 0 
          then None
          else let costs: int list = 
            List.map (fun n -> aux n (sub curr n) new_visited) unvisited_nodes
            |> List.map (fun x -> match x with | None -> None | Some x -> Some (x + cost curr n curr_dir))
            |> List.filter (fun o -> match o with | None -> false | Some _ -> true)
            |> List.map (fun o -> match o with | None -> 0 | Some x -> x)
          in
          if List.length costs = 0 
            then None
            else Some (List.fold_left min 1001 costs)
      )
  in 
  aux problem.start east NodeSet.empty;;

find_min_cost_traversal problem;;