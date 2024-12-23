
type vec2 = (int * int);;
type guy = {pos: vec2; vel: vec2};;

let _sample =
"p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3";;

let input = Day14.Input.file_contents;;

let clean = Str.global_replace (Str.regexp "[^0-9,-]") "";;

let parse_input input =
  let lines = Str.split (Str.regexp "\n") input in
  List.map (fun line ->
    let parts = Str.split (Str.regexp " ") line in
    let parts = List.map clean parts in
    let pos = Str.split (Str.regexp ",") (List.nth parts 0) in
    let vel = Str.split (Str.regexp ",") (List.nth parts 1) in
    {
      pos = (
        int_of_string (List.nth pos 0), 
        int_of_string (List.nth pos 1)
      ); 
      vel = (
        int_of_string (List.nth vel 0), 
        int_of_string (List.nth vel 1)
      )
    }
  ) lines;;

let guys = parse_input input;;

let apply_steps guys steps =
  List.map (fun guy ->
    {
      pos = (
        (fst guy.pos) + (steps * (fst guy.vel)),
        (snd guy.pos) + (steps * (snd guy.vel))
      );
      vel = guy.vel
    }
  ) guys;;

let modulo x y =
  let result = x mod y in
  if result < 0 then result + y else result;;

let wrap guys ((width, height): vec2) = 
  List.map (fun guy ->
    {
      pos = (
        modulo (fst guy.pos) width,
        modulo (snd guy.pos) height
      );
      vel = guy.vel
    }
  ) guys;;

(* let space = (11, 7);; *)
let space = (101, 103);;

let future = apply_steps guys 100 |> (fun l -> wrap l space);;

module Vec2Map = Map.Make(struct type t = vec2 let compare = compare end);;

let increment_update = (fun maybe -> match maybe with | Some(v) -> Some(v + 1) | None -> Some(1))

let guy_space_index =
  List.fold_left (fun acc guy ->
    Vec2Map.update guy.pos increment_update acc
  ) Vec2Map.empty future;;

let find_total_in_region map a b =
  let a_x, a_y = a in
  let b_x, b_y = b in
  Vec2Map.fold (fun k v acc ->
    let x, y = k in
    if x >= a_x && x < b_x && y >= a_y && y < b_y then acc + v else acc
  ) map 0;;

let find_quadrants space =
  let width, height = space in
  let half_width = width / 2 in
  let half_height = height / 2 in
  [
    ((0, 0), (half_width, half_height));
    ((half_width + 1, 0), (width, half_height));
    ((0, half_height + 1), (half_width, height));
    ((half_width + 1, half_height + 1), (width, height))
  ];;

let quadrants = find_quadrants space;;

let find_quadrant_totals quadrants =
  List.map (fun (a, b) -> find_total_in_region guy_space_index a b) quadrants;;

let solution = find_quadrant_totals quadrants |> List.fold_left ( * ) 1;;

Printf.printf "%d\n" solution;;

let total_guy_count = List.length guys;;

let tree_region_start = (0,0);;
let tree_region_end = (fst space / 2, snd space / 2);;

let build_vec2_map_string map space i =
  let width, height = space in
  let buffer = Buffer.create 1024 in
  let found = ref false in
  for y = 0 to height - 1 do
    let consecutive_count = ref 0 in
    for x = 0 to width - 1 do
      let count = match Vec2Map.find_opt (y, x) map with
      | Some(v) -> v
      | None -> 0 in
      if count = 0 then 
        consecutive_count := 0
      else 
        incr consecutive_count;
      if !consecutive_count >= 6 then found := true;
      if count = 0 then Buffer.add_char buffer ' ' else Buffer.add_string buffer (string_of_int count)
    done;
    Buffer.add_char buffer '\n';
  done;
  if not !found then "" else begin
    Buffer.add_string buffer (string_of_int i);
    Buffer.add_string buffer "\n##################################################################################################\n";
    Buffer.contents buffer
  end;;

(* let middle_start = (fst space / 3, snd space / 3);;
let middle_end = (2 * fst space / 3, 2 * snd space / 3);; *)
(* iterate until 50% of guys are found in middle region *)

let rec find_middle_region guys space i =
  let future = apply_steps guys 1 |> (fun l -> wrap l space) in
  let guy_space_index =
    List.fold_left (fun acc guy ->
      Vec2Map.update guy.pos increment_update acc) 
      Vec2Map.empty future in
  let res = build_vec2_map_string guy_space_index space i in
  print_string res;
  let total_in_middle = find_total_in_region guy_space_index tree_region_start tree_region_end in
  if total_in_middle >= total_guy_count * 5 / 10 then future else find_middle_region future space (i+1);;

let _final_state = find_middle_region guys space 0;;

(* print_vec2_map guy_space_index space;; *)

(* 2251 has interesting / weird noise *)
(* 2231 has interesting / weird state *)
(* 3962 *)
(* 3935 *)

(* 905 *)
(* 872 *)

(* 515 *)