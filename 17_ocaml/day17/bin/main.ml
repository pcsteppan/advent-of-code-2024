let rec pow a b =
  match b with | 0 -> 1 | 1 -> a | _ -> a * pow a (b - 1)

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
  
  let print c =
    Printf.printf "A: %d, B: %d, C: %d, IP: %d, Program: [%s], Output: [%s]\n"
      c.a c.b c.c c.ip
      (String.concat ";" (List.map string_of_int c.program))
      (String.concat ";" (List.map string_of_int (List.rev c.output)))

  let combo_operand o c =
    match o with
    | 0 | 1 | 2 | 3 -> o
    | 4 -> c.a
    | 5 -> c.b
    | 6 -> c.c
    | _ -> failwith ("Unexpected operand: " ^ Int.to_string o)
  
  let adv operand c = let quotient = c.a / (pow 2 operand) in {c with a = quotient}
  let bdv operand c = let quotient = c.a / (pow 2 operand) in {c with b = quotient}
  let cdv operand c = let quotient = c.a / (pow 2 operand) in {c with c = quotient}
  
  let bxl operand c = let b_xor = c.b lxor operand in {c with b = b_xor}
  let bst operand c = let b' = operand mod 8 in {c with b = b'}
  let jnz operand c = if c.a = 0 then c else {c with ip = operand - 2; }
  let bxc _operand c = let b' = c.b lxor c.c in {c with b = b'}
  
  let out operand c = let out_val = operand mod 8 in {c with output = out_val :: c.output}

  let exec opcode operand c =
    let combo = combo_operand operand c in 
    match opcode with
    | 0 -> adv combo c
    | 1 -> bxl operand c
    | 2 -> bst combo c
    | 3 -> jnz operand c
    | 4 -> bxc operand c
    | 5 -> out combo c
    | 6 -> bdv combo c
    | 7 -> cdv combo c
    | _ -> failwith ("Unexpected opcode: " ^ Int.to_string opcode)

  let cycle c =
    let operator = List.nth c.program c.ip in
    let operand = List.nth c.program (c.ip + 1) in
    (* Printf.printf "executing %d %d\n" operator operand; *)
    let c' = exec operator operand c in
    (* print c; *)
    {c' with ip = c'.ip + 2}
    
  let rec run c =
    if c.ip >= List.length c.program 
      then {c with output = List.rev c.output} 
      else run (cycle c)

  let rec find_quine c =
    let c_complete = run c in
    Printf.printf "%d\r" c.a;
    if c.program = c_complete.output then c.a
    else find_quine ({empty with a = c.a + 1; program = c.program})
end

let problem = { Computer.empty with a = 21539243; program = [2;4;1;3;7;5;1;5;0;3;4;1;5;5;3;0]; }
let finished = Computer.run problem;;

Computer.print finished;;

let output_string = String.concat "," (List.map string_of_int finished.output) in 
Printf.printf "Output: %s\n" output_string;;

let problem2 = { problem with a = 66559430; };;
let finished2 = Computer.find_quine problem2;;

print_endline @@ string_of_int finished2;;