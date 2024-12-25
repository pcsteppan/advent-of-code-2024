type dir = North | East | South | West;;
type obj = Guy | Wall | Block;;

module PosOrd = struct
  type t = (int * int)
  let compare = compare
end

module Scene = Map.Make(PosOrd);;

type scene = {
  guy_pos: (int * int);
  scene: obj Scene.t
};;

type problem = {
  scene: scene;
  directions: dir list
};;

let _sample =
"########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<";;

let parse_scene (input : string) : scene =
  print_endline input;
  let blocks_and_walls: (obj*int*int) list = 
    input |> String.split_on_char '\n' 
    |> List.mapi (fun i row -> 
      row |> String.to_seq 
      |> List.of_seq 
      |> List.mapi (fun j c -> 
        match c with
        | 'O' -> Some (Block, i, j)
        | '#' -> Some (Wall, i, j)
        | '@' -> Some (Guy, i, j)
        | _ -> None
      )
    )
    |> List.flatten
    |> List.filter_map (fun x -> x)
  in
  let scene = List.fold_left (fun acc (o, i, j) -> 
    if o <> Guy 
      then Scene.add (i, j) o acc 
      else acc
  ) Scene.empty blocks_and_walls
  in
  let guy_pos = List.find_map (fun (o, i, j) -> 
    if o = Guy then Some (i, j) else None
  ) blocks_and_walls
  in
  match guy_pos with
  | Some pos -> { guy_pos = pos; scene = scene }
  | None -> failwith "Error: No guy found in scene.";;

let split_on_string str delimiter =
  let re = Str.regexp_string delimiter in
  Str.split re str

let parse_directions (input : string) : dir list =
  input |> String.to_seq 
  |> List.of_seq 
  |> List.filter_map (fun c -> 
    match c with
    | '^' -> Some North
    | '>' -> Some East
    | 'v' -> Some South
    | '<' -> Some West
    | _ -> None
  );;

let parse_problem (input : string) : problem =
  let parts = split_on_string input "\n\n" in
  let scene = parse_scene (List.nth parts 0) in
  let directions = parse_directions (List.nth parts 1) in
  { scene = scene; directions = directions };;

let add_pos (x, y) (dx, dy) =
  (x + dx, y + dy)

let update_pos pos dir =
  let change = match dir with 
  | North -> (-1,  0)
  | East ->  ( 0,  1)
  | South -> ( 1,  0)
  | West ->  ( 0, -1) in
  add_pos pos change

let move scene dir: scene = 
  let rec aux (scene: scene) (o: obj) (new_pos: (int*int)) (dir: dir) : scene option = 
    let lookup : obj option = Scene.find_opt new_pos scene.scene
    in
    let new_scene = if o = Guy 
      then { guy_pos = new_pos; scene = Scene.remove new_pos scene.scene } 
      else { scene with scene = Scene.add new_pos Block scene.scene }
    in
    let next_pos = update_pos new_pos dir
    in
    match lookup with
    | None -> Some new_scene
    | Some(colliding_object) -> 
      match colliding_object with
      | Wall -> None
      | Block -> aux new_scene Block next_pos dir
      | Guy -> failwith "Error: Other guy found in scene." 
  in
  let new_pos = update_pos scene.guy_pos dir
  in
  aux scene Guy new_pos dir |> Option.value ~default:scene;;

let get_all_blocks scene = Scene.fold (fun pos obj acc -> 
  match obj with
  | Block -> pos :: acc
  | _ -> acc
) scene [];;

let sum_blocks scene = List.fold_left 
  (fun acc (row, col) -> acc + row*100 + col) 
  0 (get_all_blocks scene);;

let part1 (input : string) : int =
  let problem = parse_problem input in
  let solved_problem = List.fold_left 
    (fun scene dir -> move scene dir) 
    problem.scene problem.directions 
  in
  sum_blocks solved_problem.scene;;

let input = Day15.Input.file_contents;;
Printf.printf "%d\n" (part1 input);;
