type node = char option * string
type node_value = { _key : string; is_word : bool; children : node list }

module NodeMap = Map.Make (struct
  type t = node
  let compare = compare
end)

let append_char c s =
  String.init (String.length s + 1) (fun i -> if i < String.length s then s.[i] else c)

module Trie = struct
  type trie = { root : node; map : node_value NodeMap.t }

  let empty =
    {
      root = (None, "");
      map = NodeMap.(empty |> add (None, "") { is_word = false; _key = ""; children = [] });
    }

  let _print trie =
    let rec aux node =
      let value = NodeMap.find node trie.map in
      let c, key = node in
      let () = match c with
        | None -> Printf.printf "('', %s) %b\n" key value.is_word
        | Some c -> Printf.printf "(%c, %s) %b\n" c key value.is_word
      in
      List.iter aux value.children
    in
    aux trie.root

  let add_node node is_word trie =
    let _, key = node in
    let new_map = NodeMap.add node { is_word; _key = key; children = [] } trie.map in
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
    if List.exists ((=) child) parent_value.children then trie
    else
      let new_children = child :: parent_value.children in
      let new_value = { parent_value with children = new_children } in
      let new_map = NodeMap.add parent new_value trie.map in
      { trie with map = new_map }

  let add_word word trie =
    let rec aux acc rest prev_node trie =
      match rest with
      | "" -> trie
      | _ ->
          let curr', rest' = (String.get rest 0, String.sub rest 1 (String.length rest - 1)) in
          let acc' = append_char curr' acc in
          let curr_node = (Some curr', acc') in
          let trie = try_add_node curr_node (rest' = "") trie in
          let trie = try_add_edge prev_node curr_node trie in
          aux acc' rest' curr_node trie
    in
    aux "" word trie.root trie

  let get_num_solutions word trie =
    let memo = Hashtbl.create (String.length word) in
    let rec aux string node =
      match Hashtbl.find_opt memo (string, node) with
      | Some v -> v
      | None ->
          let value = NodeMap.find node trie.map in
          let result = match string with
            | "" -> if value.is_word then 1 else 0
            | _ ->
                let c', rest' = (String.get string 0, String.sub string 1 (String.length string - 1)) in
                let child = List.find_opt (fun (c, _) -> c = Some c') value.children in
                match (child, value.is_word) with
                | Some node', false -> aux rest' node'
                | Some node', true -> aux rest' node' + aux string trie.root
                | None, false -> 0
                | None, true -> aux string trie.root
          in
          Hashtbl.add memo (string, node) result;
          result
    in
    aux word trie.root
end

let get_words input =
  List.map String.trim (String.split_on_char ',' (List.hd (String.split_on_char '\n' input)))

let get_candidates input =
  List.filter ((<>) "") (List.map String.trim (List.tl (String.split_on_char '\n' input)))

let solve input =
  let words = get_words input in
  let candidates = get_candidates input in
  let trie = List.fold_left (fun acc word -> Trie.add_word word acc) Trie.empty words in
  print_endline "processing candidates...";
  let solutions = List.map (fun c -> Trie.get_num_solutions c trie) candidates in
  let valid_candidates = List.length (List.filter ((<>) 0) solutions) in
  let total_solutions = List.fold_left (+) 0 solutions in
  Printf.printf "valid candidates: %d\ntotal solutions: %d\n" valid_candidates total_solutions
;;

let _sample =
  "r, wr, b, g, bwu, rb, gb, br\n\n\
   brwrr\n\
   bggr\n\
   gbbr\n\
   rrbgbr\n\
   ubwu\n\
   bwurrg\n\
   brgr\n\
   bbrgwb";;

solve Day19.Input.file_contents