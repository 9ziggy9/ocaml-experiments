type gate =
  | Unary  of (bool -> bool)
  | Binary of (bool -> bool -> bool)
  | Circuit of circuit
and circuit = gate list

let not_gate  : gate = Unary  (fun a -> not a)
let and_gate  : gate = Binary (fun a b -> a && b)
let or_gate   : gate = Binary (fun a b -> a || b)
let nand_gate : gate = Binary (fun a b -> not (a && b))
let nor_gate  : gate = Binary (fun a b -> not (a || b))
let xor_gate  : gate = Binary (fun a b -> a <> b)
let xnor_gate : gate = Binary (fun a b -> a = b)

(* let rec evaluate_circuit (inputs: bool list) (circuit: circuit) : bool list = *)
(*   match circuit with *)
(*   | [] -> inputs *)
(*   | Unary f :: rest -> let output = List.map f inputs *)
(*                        in evaluate_circuit output rest *)
(*   | Binary f :: rest -> *)
(*       let rec apply_binary pairs = *)
(*         match pairs with *)
(*         | a :: b :: tail -> f a b :: apply_binary tail *)
(*         | _ -> []  (\* Handle the case of an odd number of elements or empty list *\) *)
(*       in let processed_inputs = apply_binary inputs *)
(*       in evaluate_circuit processed_inputs rest *)
(*   | Circuit c :: rest -> let circuit_output = evaluate_circuit inputs c *)
(*                          in evaluate_circuit circuit_output rest *)

let print_gate_truth_table gate =
  let print_row inputs result =
    let input_str = List.map (fun b -> if b then "1" else "0") inputs |> String.concat " " in
    let result_str = if result then "1" else "0" in
    Printf.printf "%s | %s\n" input_str result_str
  in
  let eval_gate inputs =
    match gate with
    | Unary f  -> [f (List.hd inputs)]
    | Binary f -> [f (List.nth inputs 0) (List.nth inputs 1)]
    | _        -> failwith "Circuit evaluation not supported"
  in
  match gate with
  | Unary _ ->
      Printf.printf "A | RESULT\n";
      List.iter (fun a -> print_row [a] (List.hd (eval_gate [a]))) [true; false]
  | Binary _ ->
      Printf.printf "A B | RESULT\n";
      List.iter
        (fun a -> List.iter
                    (fun b -> print_row [a; b]
                                (List.hd (eval_gate [a; b]))) [true; false])
        [true; false]
  | _ ->
      Printf.printf "Circuit truth table printing not supported\n"

let test_gates () =
  print_string "not gate\n";
  print_gate_truth_table not_gate;
  print_newline ();
  print_string "and gate\n";
  print_gate_truth_table and_gate;
  print_newline ();
  print_string "or gate\n";
  print_gate_truth_table or_gate;
  print_newline ();
  print_string "nand gate\n";
  print_gate_truth_table nand_gate;
  print_newline ();
  print_string "nor gate\n";
  print_gate_truth_table nor_gate;
  print_newline ();
  print_string "xor gate\n";
  print_gate_truth_table xor_gate;
  print_newline ();
  print_string "xnor gate\n";
  print_gate_truth_table xnor_gate;;

let () =
  test_gates ()
