
module Trie = struct
  type node = Node of char * bool * node list
  type 'a trie = node list

  let empty = []

  let from_word (word: string) : 'a trie =
    let letters = String.to_seq word |> List.of_seq in
    let rec aux chars : node option =
      match chars with
      | [] -> None
      | c :: remaining -> match (aux remaining) with
        | Some n -> Some (Node (c, false, [n]))
        | None -> Some (Node (c, true, []))
    in
    [aux letters |> Option.get]

end

let test = Trie.from_word "abc"