type node = char option * string
type node_value = { _key : string; is_word : bool; children : node list }

module NodeMap = Map.Make (struct
  type t = node

  let compare = compare
end)

let append_char c s =
  List.of_seq (String.to_seq s) @ [ c ] |> List.to_seq |> String.of_seq

module Trie = struct
  type trie = { root : node; map : node_value NodeMap.t }

  let empty =
    {
      root = (None, "");
      map =
        NodeMap.(
          empty |> add (None, "") { is_word = false; _key = ""; children = [] });
    }

  let _print trie =
    let rec aux node =
      let value = NodeMap.find node trie.map in
      let c, key = node in

      let () =
        match c with
        | None -> Printf.printf "('', %s) %b\n" key value.is_word
        | Some c -> Printf.printf "(%c, %s) %b\n" c key value.is_word
      in
      List.iter aux value.children
    in
    aux trie.root

  (*
  Adds a word to the given trie and returns a new trie.

  abcde:

   "" ''  "abcde" // no previous value, do nothing
   "a" 'a' "bcde" // add (Some a, "a", false, []) if it does not exist // add prev -> curr in map
   "ab" 'b' "cde" // add (Some b, "ab", false, []) if it does not exist // add prev -> curr in map
   "abc" 'c' "de"
   "abcd" 'd' "e"
   "abcde" 'e' ""
  *)

  let add_node node is_word trie =
    let _, key = node in
    let new_map =
      NodeMap.add node { is_word; _key = key; children = [] } trie.map
    in
    { trie with map = new_map }

  let try_add_node node is_word trie =
    match NodeMap.find_opt node trie.map with
    | Some node_in_map ->
        if is_word && not node_in_map.is_word then
          let new_value = { node_in_map with is_word = true } in
          let new_map = NodeMap.add node new_value trie.map in
          { trie with map = new_map }
        else trie
    | None -> add_node node is_word trie

  let try_add_edge parent child trie =
    let parent_value = NodeMap.find parent trie.map in
    match List.find_opt (fun n -> n = child) parent_value.children with
    | Some _ -> trie
    | None ->
        let new_children = child :: parent_value.children in
        let new_value = { parent_value with children = new_children } in
        let new_map = NodeMap.add parent new_value trie.map in
        { trie with map = new_map }

  let add_word (word : string) (trie : trie) =
    let rec aux (acc : string) (rest : string) (prev_node : node) (trie : trie)
        =
      match rest with
      | "" -> trie
      | _ ->
          let curr', rest' =
            (String.get rest 0, String.sub rest 1 (String.length rest - 1))
          in
          let acc' = append_char curr' acc in
          let curr_node : node = (Some curr', acc') in
          let trie = try_add_node curr_node (rest' = "") trie in
          let trie = try_add_edge prev_node curr_node trie in
          aux acc' rest' curr_node trie
    in
    aux "" word trie.root trie

  let get_num_solutions word trie : int =
    let memo = Hashtbl.create (String.length word) in
    let rec aux string node =
      (* print_endline string; *)
      if node = trie.root && Hashtbl.mem memo string then (
        print_endline "hit";
        0)
      else
        let value = NodeMap.find node trie.map in
        match string with
        | "" -> if value.is_word then 1 else 0
        | _ ->
            let c', rest' =
              ( String.get string 0,
                String.sub string 1 (String.length string - 1) )
            in
            (* let test = List.filter (fun (c, _) -> c = Some c') value.children in
            let _test_length = List.length test in *)
            (* if test_length > 1 then print_endline "more than one child"; *)
            let child =
              List.find_opt (fun (c, _) -> c = Some c') value.children
            in
            let result =
              match (child, value.is_word) with
              | Some node', false -> aux rest' node'
              | Some node', true -> aux rest' node' + aux string trie.root
              | None, false -> 0
              | None, true -> aux string trie.root
            in
            print_endline string;
            if result = 0 && node = trie.root then (
              print_endline string;
              Printf.printf "add invalid string: %s\n" string;
              Hashtbl.add memo string true;
              0)
            else result
    in
    aux word trie.root
end

let _sample =
  "r, wr, b, g, bwu, rb, gb, br\n\n\
   brwrr\n\
   bggr\n\
   gbbr\n\
   rrbgbr\n\
   ubwu\n\
   bwurrg\n\
   brgr\n\
   bbrgwb"

let get_words input =
  input |> String.split_on_char '\n' |> List.hd |> String.split_on_char ','
  |> List.map String.trim

let get_candidates input =
  input |> String.split_on_char '\n' |> List.tl |> List.map String.trim
  |> List.filter (fun s -> s <> "")

let solve input =
  let words = get_words input in
  let candidates = get_candidates input in
  let trie =
    List.fold_left (fun acc word -> Trie.add_word word acc) Trie.empty words
  in
  Trie._print trie;
  print_endline "processing candidates...";
  let valid_candidates =
    List.filter (fun c -> Trie.get_num_solutions c trie > 0) candidates
  in
  Printf.printf "valid candidates: %d\n" (List.length valid_candidates)
;;

(* let _sample = "a,abb,cbba\n\ncbb";; *)

solve Day19.Input.file_contents

(* 272 is too low *)
(* 292 is incorrect *)
(* 298 is too high *)
