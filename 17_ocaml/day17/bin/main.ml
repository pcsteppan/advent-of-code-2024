let ( <<< ) a b = Int.shift_left a b
let ( >>> ) a b = Int.shift_right a b
let ( & ) a b = a land b

module Computer = struct
  type 'a computer = {
    a : int;
    b : int;
    c : int;
    ip : int;
    program : int list;
    output : int list;
  }

  let empty = { a = 0; b = 0; c = 0; ip = 0; program = []; output = [] }

  let _print c =
    Printf.printf "A: %d, B: %d, C: %d, IP: %d\nProgram:\t%s\nOutput:\t\t%s\n"
      c.a c.b c.c c.ip
      (String.concat " " (List.map string_of_int c.program))
      (String.concat " " (List.map string_of_int c.output))

  let special_arg arg c =
    match arg with
    | 0 | 1 | 2 | 3 -> arg
    | 4 -> c.a
    | 5 -> c.b
    | 6 -> c.c
    | _ -> failwith ("Unexpected arg: " ^ Int.to_string arg)

  let adv arg c = { c with a = c.a >>> arg }
  let bdv arg c = { c with b = c.a >>> arg }
  let cdv arg c = { c with c = c.a >>> arg }
  let bxl arg c = { c with b = c.b lxor arg }
  let bst arg c = { c with b = arg & 7 }
  let jnz arg c = if c.a = 0 then c else { c with ip = arg - 2 }
  let bxc _arg c = { c with b = c.b lxor c.c }
  let out arg c = { c with output = (arg & 7) :: c.output }

  let exec opcode arg c =
    match opcode with
    | 1 -> bxl arg c
    | 3 -> jnz arg c
    | 4 -> bxc arg c
    | _ -> (
        let s_arg = special_arg arg c in
        match opcode with
        | 0 -> adv s_arg c
        | 2 -> bst s_arg c
        | 5 -> out s_arg c
        | 6 -> bdv s_arg c
        | 7 -> cdv s_arg c
        | _ -> failwith ("Unexpected opcode: " ^ Int.to_string opcode))

  let cycle c =
    let operator = List.nth c.program c.ip in
    let arg = List.nth c.program (c.ip + 1) in
    let c' = exec operator arg c in
    { c' with ip = c'.ip + 2 }

  let rec run c =
    if c.ip >= List.length c.program then { c with output = List.rev c.output }
    else run (cycle c)

  let find_quine c =
    let rec aux c a i =
      let c' = { empty with a; program = c.program } in
      let result = run c' in
      if result.output = c.program then Some a
      else if i > List.length c.program then None
      else
        let start = List.length c.program - i in
        let slice = List.filteri (fun idx _ -> idx >= start) c.program in
        if i = 0 || result.output = slice then
          List.fold_left
            (fun acc x ->
              match acc with
              | Some _ -> acc
              | None -> aux c ((a <<< 3) + x) (i + 1))
            None
            (List.init 8 (fun x -> x))
        else None
    in
    aux c 0 0
end

let _sample = { Computer.empty with program = [ 0; 3; 5; 4; 3; 0 ] }

let problem =
  {
    Computer.empty with
    a = 21539243;
    program = [ 2; 4; 1; 3; 7; 5; 1; 5; 0; 3; 4; 1; 5; 5; 3; 0 ];
  }
;;

print_endline "Part 1: ";
(Computer.run problem).output |> List.iter (fun x -> Printf.printf "%d," x);

print_endline "\n\nPart 2: ";
Computer.find_quine problem
|> Option.map Int.to_string |> Option.get |> print_endline
