let rec rev_list1 lst = match lst with
  | []    -> []
  | x::xs -> rev_list1 xs @ [x]

let is_palindrome lst = lst = (rev_list1 lst)

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | []          -> acc
    | One x :: t  -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in List.rev (aux [] lst)

let rec compress = function
    (* Note that the alias t is bound to THE ENTIRE LIST, i.e. *)
    (* | a :: ((b :: _) as t) -> if a = b then compress t else a :: compress t *)
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller


let rec rev_list ( lst : 'a list ) : 'a list =
  let rec append x xs = match xs with
    | []           -> [x]
    | head :: tail -> head :: append x tail
  in match lst with
     | [] -> []
     | head :: tail -> append head (rev_list tail)

let rle_simple list =
    let rec aux count acc = function
      | [] -> []
      | [x] -> (count + 1, x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count + 1, a) :: acc) t in
    List.rev (aux 0 [] list)

let pack list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    rev_list (aux [] [] list)

let rle_1 list =
  List.map (fun sublist -> (List.length sublist, List.hd sublist)) (pack list)

type 'a rle_encoding =
    | One of 'a
    | Many of int * 'a

let rle_variants3 list =
    let rec aux count acc = function
      | [] -> List.rev acc
      | [x] -> List.rev ((One x) :: acc)
      | a :: (b :: _ as t) -> if a = b
          then aux (count + 1) acc t
          else aux 0 ((if count = 0 then One a else Many (count + 1, a)) :: acc) t
    in aux 0 [] list

let rle_variants2 list =
    let rec aux count acc = function
      | [] -> acc
      | [x] -> One x :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else Many (count + 1, a) :: aux 0 acc t in
    List.rev (aux 0 [] list)

let rle l =
  let create_tuple cnt elem =
    if cnt = 1 then One elem
    else Many (cnt, elem) in
  let rec aux count acc = function
    | [] -> []
    | [x] -> (create_tuple (count + 1) x) :: acc
    | hd :: (snd :: _ as tl) ->
        if hd = snd then aux (count + 1) acc tl
        else aux 0 ((create_tuple (count + 1) hd) :: acc) tl in
    List.rev (aux 0 [] l)

let rec un_rle_model encodings =
  match encodings with
  | [] -> []
  | (One x) :: xs -> x :: (un_rle_model xs)
  | (Many (count, x)) :: xs -> List.init count (fun _ -> x) @ (un_rle_model xs)

let un_rle list =
  let rec many acc n x =
    if n = 0 then acc else many (x :: acc) (n - 1) x
  in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (many acc n x) t
  in
    aux [] (List.rev list)

(* let rec replicate l count = match l with *)
(*   | x :: xs -> x :: replicate xs (count + 1) *)
(*   | [x]     -> x :: [] *)
(*   | _       -> [] *)

let rec dup_1 x n = if n < 1 then []
                  else if n = 1 then [x]
                  else [x] @ dup_1 x (n - 1)

let replicate_1 lst count =
  List.flatten (List.map (fun x -> dup_1 x count) lst)

let dup x n =
  let rec aux acc count = if count < 1 then acc
                          else if count = 1 then x :: acc
                          else x :: (aux acc (count - 1)) in
  aux [] n

let replicate_solution list n =
    let rec prepend n acc x =
      if n = 0 then acc else prepend (n-1) (x :: acc) x in
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (prepend n acc h) t in
    (* This could also be written as:
       List.fold_left (prepend n) [] (List.rev list) *)
    aux [] (List.rev list)

let replicate lst n =
  let rec aux l m acc = match l with
  | x :: xs -> aux xs m (dup x m @ acc)
  | []      -> acc
  in List.rev (aux lst n [])

let drop lst n =
  let rec aux i = function
    | []    -> []
    | x::xs -> if i = n
               then aux 0 xs
               else x :: (aux (i + 1) xs)
  in aux 0 lst

let split_solution list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (h :: acc) t 
    in aux n [] list

let split lst f_len =
  let rec aux_fst i acc1 = function
    | []      -> []
    | x :: xs -> if i = f_len then acc1
                 else aux_fst (i + 1) (acc1 @ [x]) xs
  in
  let rec aux_snd j acc2 = function
    | []      -> acc2
    | x :: xs -> if j <= f_len then aux_snd (j + 1) acc2 xs
                 else aux_snd (j + 1) (acc2 @ [x]) xs
  in
  (aux_fst 0 [] lst, aux_snd 0 [] lst)

let rec slice_model lst left right =
  if left = 0 then
    if right = 0 then
      []
    else
      match lst with
      | [] -> []
      | hd::tl -> hd :: slice_model tl (left - 1) (right - 1)
  else
    match lst with
    | [] -> []
    | _::tl -> slice_model tl (left - 1) (right - 1)

let slice lst left right = 
  let rec aux acc i = function
    | []      -> acc
    | x :: xs -> if (i < left || i > right )
                 then aux acc (i + 1) xs
                 else aux (acc @ [x]) (i + 1) xs
  in aux [] 0 lst

let split_model lst i =
  let left = slice lst 0 (i - 1) in
  let right = slice lst i (List.length lst) in
  (left, right)

let split_alt lst idx =
  if idx < 0
  then let idx = List.length lst + idx
       in slice lst 0 idx, slice lst (idx + 1) (List.length lst)
  else slice lst 0 (idx - 1), slice lst idx (List.length lst)

let rotate lst n =
  let len = List.length lst in
  let n = if n < 0 then len + n else n in
  let first = slice lst 0 (n - 1) in
  let second = slice lst n len in
  second @ first

let rec remove_at_solution idx = function
  | []      -> []
  | x :: xs -> if idx = 0 then xs else x :: remove_at_solution (idx - 1) xs

let remove_at idx lst =
  let rec aux acc target = function
    | []      -> acc
    | x :: xs ->
       if target = idx then aux acc (target + 1) xs
       else aux (acc @ [x]) (target + 1) xs
  in aux [] 0 lst

let rec insert_at el idx =  function
  | []             -> [el]
  | (x :: xs) as l -> if idx = 0 then el :: l
                      else x :: insert_at el (idx - 1) xs

let rec range from until =
  let move = fun n -> if from < until then n + 1 else n - 1 in
  if from = until then [until] else from :: range (move from) until

let range_tail_recursive from until =
  let rec aux from until acc =
    let move = if from < until then (fun n -> n + 1) else (fun n -> n - 1) in
    if from = until then until :: acc
    else aux (move from) until (from :: acc)
  in List.rev (aux from until [])

let rand_select_model l n =
  let rec aux acc n =
    if n = 0 then acc
    else
      let len = List.length l in
      let i = Random.int len in
      let x = List.nth l i in
      if List.mem x acc then aux acc n
      else aux (x :: acc) (n - 1)
  in aux [] n

let rand_select list n =
    let rec extract acc n = function
      | [] -> raise Not_found
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux n acc list len =
      if n = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (n - 1) (picked :: acc) rest (len - 1)
    in
    let len = List.length list in
      aux (min n len) [] list len

type bool_expr =
    | Ident of string
    | Not   of bool_expr
    | And   of bool_expr * bool_expr
    | Or    of bool_expr * bool_expr
    | Xor   of bool_expr * bool_expr
    | Nand  of bool_expr * bool_expr
    | Nor   of bool_expr * bool_expr

let rec eval2 identA valA identB valB = function
  | Ident x      -> if x = identA then valA
                    else if x = identB then valB
                    else failwith "Panic: invalid var in expr"
  | Not e        -> not (eval2 identA valA identB valB e)
  | And (e1, e2) -> eval2 identA valA identB valB e1
                    && eval2 identA valA identB valB e2
  | Or  (e1, e2) -> eval2 identA valA identB valB e1
                    || eval2 identA valA identB valB e2
  | Xor  (e1, e2) -> eval2 identA valA identB valB e1
                     <> eval2 identA valA identB valB e2
  | Nand (e1, e2) -> not (eval2 identA valA identB valB e1
                          && eval2 identA valA identB valB e2)
  | Nor  (e1, e2) -> not (eval2 identA valA identB valB e1
                          || eval2 identA valA identB valB e2)


(* A more Haskell-like syntax for type annotation. *)
let table2 : string -> string -> bool_expr -> (bool * bool * bool) list =
  fun ident1 ident2 expr -> [
      (true,  true,  eval2 ident1 true  ident2 true  expr);
      (true,  false, eval2 ident1 true  ident2 false expr);
      (false, true,  eval2 ident1 false ident2 true  expr);
      (false, false, eval2 ident1 false ident2 false expr);
    ]

let print_table2 table2 =
  let header = "| A | B | out |" in
  let bar = String.make (String.length header) '-' in
  let rec print_rows = function
    | [] -> ()
    | ((Ident a, Ident b, c) :: rows) ->
      let c_str = if c then "1" else "0" in
      let row = Printf.sprintf "| %s | %s | %s |" a b c_str in
      print_endline row;
      print_rows rows
  in
  print_endline bar;
  print_endline header;
  print_endline bar;
  print_rows table2;
  print_endline bar


let test_gate expected name expr =
  let actual = table2 "a" "b" expr in
  if expected = actual then
    Printf.printf "Testing %s gate: PASSED\n" name
  else
    Printf.printf "Testing %s gate: FAILED\n" name

let run_tests () =
  test_gate [
    (true, true, true);
    (true, false, false);
    (false, true, false);
    (false, false, false);
  ] "And" (And (Ident "a", Ident "b"));

  test_gate [
    (true, true, true);
    (true, false, true);
    (false, true, true);
    (false, false, false);
  ] "Or" (Or (Ident "a", Ident "b"));

  test_gate [
    (true, true, false);
    (true, false, true);
    (false, true, true);
    (false, false, false);
  ] "Xor" (Xor (Ident "a", Ident "b"));

  test_gate [
    (true, true, false);
    (true, false, true);
    (false, true, true);
    (false, false, true);
  ] "Nand" (Nand (Ident "a", Ident "b"));

  test_gate [
    (true, true, false);
    (true, false, false);
    (false, true, false);
    (false, false, true);
  ] "Nor" (Nor (Ident "a", Ident "b"));;

type truth_table2 = (bool * bool * bool) list

let apply_truth_table2 table2 =
  fun a b ->
    match List.assoc (a, b) table2 with
    | Some c -> c
    | None -> failwith "Panic: invalid input in apply_truth_table2"
