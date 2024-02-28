type dict = {
  mutable data : (string, int list) Hashtbl.t;
}

let create_dict lst =
  let dict = Hashtbl.create 10 in
  List.iter (fun (key, value) ->
    match Hashtbl.find_opt dict key with
    | Some values -> Hashtbl.replace dict key (value :: values)
    | None -> Hashtbl.add dict key [value]
  ) lst;
  { data = dict }
