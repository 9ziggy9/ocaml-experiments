type signal = High | Low

type gate = signal list -> signal

let assert_gate_inputs expected_inputs gate inputs =
  if List.length inputs = expected_inputs then gate inputs
  else failwith "Runtime assertion failure: incorrect inputs to gate"

let and_gate = assert_gate_inputs 2 (fun inputs -> match inputs with
  | [High; High] -> High
  | _            -> Low)

let or_gate = assert_gate_inputs 2 (fun inputs -> match inputs with
  | [Low; Low] -> Low
  | _          -> High)

let not_gate inputs = assert_gate_inputs 1 (fun inputs -> match inputs with
  | [High] -> Low
  | _      -> High)

type circuit = signal list -> signal list

let example_circuit inputs =
  match inputs with
  | [input1; input2] ->
    let output1 = and_gate [input1; input2] in
    let output2 = or_gate [input1; input2] in
    [output1; output2]
  | _ -> failwith "This circuit expects two inputs"
