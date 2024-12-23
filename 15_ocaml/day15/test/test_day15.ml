let test expr = if not expr then raise (Failure "Test failed.") else ();;

test true;;
test false;;

type dir = North | East | South | West;;
type obj = Wall | Block | Empty;;

type scene = {
  guy_pos: (int * int);
  scene: obj list;
};;

let add_pos (x, y) (dx, dy) =
  (x + dx, y + dy)

let update_pos pos dir =
  let change = match dir with 
  | North -> (-1,  0)
  | East ->  ( 0,  1)
  | South -> ( 1,  0)
  | West ->  ( 0, -1) in
  add_pos pos change

let move (scene : scene) (dir: dir) : scene option =
  let rec aux (scene: scene) (dir: dir) (piece: obj) (pos: int) : scene option =
    let new_obj = List.nth_opt scene.scene pos in
    match new_obj with
    | None -> None
    | Some(Wall) -> None
    | Some(Block) ->
      let new_scene = {
        (* creates an empty where there was a block, and 'moves' that block by recursively calling aux *)
        guy_pos = 
      }
    | Some(Empty) -> 
      let new_scene = {
        guy_pos = (fst scene.guy_pos, snd scene.guy_pos + 1);
        scene = List.mapi (fun i obj -> if i = pos then Empty else obj) scene.scene;
      } in
      Some(new_scene)

let scene : scene = {
  guy_pos = (0, 0);
  scene = [Empty; Empty; Block; Empty; Block; Empty; Wall;];
};;



let scene_move1 = {
  guy_pos = (0, 1);
  scene = [Empty; Empty; Block; Empty; Block; Empty; Wall;];
}

let scene_move2 = {
  guy_pos = (0, 2);
  scene = [Empty; Empty; Empty; Block; Block; Empty; Wall;];
}

let scene_move3 = {
  guy_pos = (0, 3);
  scene = [Empty; Empty; Empty; Empty; Block; Block; Wall;];
}

let scene_move4 = {
  guy_pos = (0, 3);
  scene = [Empty; Empty; Empty; Empty; Block; Block; Wall;];
}