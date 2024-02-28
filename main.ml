type dict = {
  mutable data : (string, int) Hashtbl.t;
}

let create_dict lst =
  let rec helper acc = function
    | [] -> acc
    | (key, value) :: tl ->
      begin
        Hashtbl.add acc.data key value;
        helper acc tl
      end
  in
  let dict = { data = Hashtbl.create 10 } in
  helper dict lst

let () =
  let lst = [("a", 1); ("b", 2); ("c", 3)] in
  let dict = create_dict lst in
  Printf.printf "%d\n" (Hashtbl.find dict.data "a");  (* prints 1 *)
  Printf.printf "%d\n" (Hashtbl.find dict.data "b");  (* prints 2 *)
  Printf.printf "%d\n" (Hashtbl.find dict.data "c");  (* prints 3 *)
