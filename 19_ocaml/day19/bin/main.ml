type node = char option * string
type node_value = { is_word : bool; children : node list }

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
      map = NodeMap.(empty |> add (None, "") { is_word = true; children = [] });
    }

  let print trie =
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
    let new_map = NodeMap.add node { is_word; children = [] } trie.map in
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
    let rec aux (acc : string) (curr : char option) (rest : string)
        (prev_node : node) (trie : trie) =
      let () =
        match curr with
        | Some c -> Printf.printf "%s %c %s\n" acc c rest
        | None -> Printf.printf "%s '' %s\n" acc rest
      in

      match rest with
      | "" ->
          print_endline "done";
          trie
      | _ ->
          let curr', rest' =
            (String.get rest 0, String.sub rest 1 (String.length rest - 1))
          in
          Printf.printf "\ncurr: %c rest: %s\n" curr' rest';
          let acc' = append_char curr' acc in
          let curr_node : node = (Some curr', acc') in
          let trie = try_add_node curr_node (rest' = "") trie in
          let trie = try_add_edge prev_node curr_node trie in
          aux acc' (Some curr') rest' curr_node trie
    in
    aux "" None word trie.root trie
end

let a = 4

let test =
  Trie.empty |> Trie.add_word "a" |> Trie.add_word "abc" |> Trie.add_word "bxy"
  |> Trie.add_word "b"
;;

(* |> Trie.add_word "xyz" |> Trie.add_word "ab";; *)

Trie.print test
