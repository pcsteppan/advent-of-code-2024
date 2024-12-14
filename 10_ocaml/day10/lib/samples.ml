let sample = 
"...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9";;

let sample2 =
"..90..9
...1.98
...2..7
6543456
765.987
876....
987....";;

let sample3 =
"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732";;

let sample4 =
".....0.
..4321.
..5..2.
..6543.
..7..4.
..8765.
..9....";;

let read_file name : string =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [] |> String.concat "\n";;

let file_contents = read_file "./bin/input.txt";;