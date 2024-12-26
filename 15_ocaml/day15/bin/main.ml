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

let parse_double_wide (input: string) : problem =
  let parts = split_on_string input "\n\n" in
  let scene = parse_scene (List.nth parts 0) in
  let double_wide_scene = Scene.fold (fun pos obj acc -> 
    let (row, col) = pos in
    let new_pos = (row, col * 2) in
    Scene.add new_pos obj acc
  ) scene.scene Scene.empty
  in
  let guy_in_wide_space_pos = let (row, col) = scene.guy_pos in (row, col * 2)
  in
  let directions = parse_directions (List.nth parts 1)
  in
  { 
    scene = { 
      guy_pos = guy_in_wide_space_pos; 
      scene = double_wide_scene 
    }; 
    directions = directions 
  };;

let combine_option_lists (lists: 'a list option list) : 'a list option =
  let open Option in
  List.fold_left (fun acc opt_list ->
    match acc, opt_list with
    | None, _ | _, None -> None
    | Some acc_list, Some lst -> Some (acc_list @ lst)
  ) (Some []) lists

(* 
  Because the guy can push a double-wide block, and that block can in turn push two double-wide blocks,
  we have to query different positions in the map to see if the guy and blocks collide.

  Here are the two cases of a guy and a block that would interact if the guy moved north:
  1  2  ^
  [] [] |  
  @   @ |

  Here are the three cases of how a block and a block would interact if the block was pushed north:
  1   2   3  ^
    [] [] []  |
  []  []  [] |

  And for the purpose of this program, we are only considering the left-most part of the block in the spatial map:
  1  2  ^
  O. O. |
  @   @ |

  1   2   3  ^
    O. O. O.  |
  O.  O.  O. |

  So for NORTH and SOUTH, the guy has two offsets to check, and for the block we have three offsets to check.

  For EAST and WEST, since the blocks and walls height did not change in part 2, we only have to check one offset.
  However, we still have to account for the double-width of blocks and walls in the spatial map.

  @[]  | @[][]##
  []@  | ##[][]@

  @O.  | @O.O.#. 
  O.@  | #.O.O.@

  The guy would collide if he moves east or west, so we can see that the west offset needs to skip an extra column to check for collisions.
*)
let get_queries (pos: (int*int)) (dir: dir) (isGuy: bool) : (int*int) list =
  let (row, col) = pos in
  match isGuy with
  | true -> 
    (
      match dir with
      | North -> [(row - 1, col); (row - 1, col - 1)]
      | East -> [(row, col + 1)]
      | South -> [(row + 1, col); (row + 1, col - 1)]
      | West -> [(row, col - 2)]
    )
  | false ->
    (
      match dir with
      | North -> [(row - 1, col - 1); (row - 1, col); (row - 1, col + 1)]
      | East -> [(row, col + 2)]
      | South -> [(row + 1, col - 1); (row + 1, col); (row + 1, col + 1)]
      | West -> [(row, col - 2)]
    )

let map_kvps (scene: scene) (f: ((int*int) * obj) -> ((int*int) * obj)) : scene =
  let new_scene = Scene.fold (fun pos obj acc -> 
    let (key, value) = f (pos, obj)
    in
    Scene.add key value acc
  ) scene.scene Scene.empty
  in
  { guy_pos = scene.guy_pos; scene = new_scene }

let move2 (scene: scene) (dir: dir) : scene =
  let rec aux (scene: scene) new_pos dir : (int * int) list option =
    let lookup : obj option = Scene.find_opt new_pos scene.scene
    in
    match lookup with
    | None -> Some([])
    | Some(colliding_object) -> 
      match colliding_object with
      | Wall -> None
      | Block -> 
        get_queries new_pos dir false
        |> List.map (fun next_pos -> aux scene next_pos dir)
        |> combine_option_lists
        |> Option.map (fun lst -> new_pos :: lst)
      | Guy -> failwith "Error: Other guy found in scene." 
  in
  let all_positions_to_move = get_queries scene.guy_pos dir true
  |> List.map (fun next_pos -> aux scene next_pos dir)
  |> combine_option_lists
  in
  match all_positions_to_move with
  | None -> scene
  | Some positions -> 
    let new_scene = map_kvps scene (fun (pos, obj) -> 
        if List.mem pos positions
          then ((update_pos pos dir), obj)
          else (pos, obj)
    )
    in 
    { guy_pos = update_pos scene.guy_pos dir; scene = new_scene.scene };;

let _print_scene (scene : scene) = 
  let (max_row, max_col) = Scene.fold (fun (row, col) _ (max_row, max_col) -> 
    (max row max_row, max col max_col)
  ) scene.scene (0, 0)
  in
  for row = 0 to max_row do
    for col = 0 to max_col+1 do
      let obj = Scene.find_opt (row, col) scene.scene
      in
      let prev_object = Scene.find_opt (row, col-1) scene.scene
      in
      if scene.guy_pos = (row, col) then Printf.printf "@"
      else match obj with
      | None -> if prev_object = None then Printf.printf "."
      | Some(Block) -> Printf.printf "[]"
      | Some(Wall) -> Printf.printf "##"
      | Some(Guy) -> ();
    done;
    Printf.printf "\n"
  done;;
    
let _large_sample = 
"##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

let _smaller_sample =
"###
#@#
#O#
#O#
#.#
###

v"

let _print_dir dir = 
  match dir with
  | North -> print_endline "North"
  | East -> print_endline "East"
  | South -> print_endline "South"
  | West -> print_endline "West"

let part2 (input: string) : int =
  let problem = parse_double_wide input in
  let solved_problem = List.fold_left move2 problem.scene problem.directions 
  in
  sum_blocks solved_problem.scene;;

Printf.printf "%d\n" (part2 input);;


