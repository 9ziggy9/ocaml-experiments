let rec rev_list1 lst = match lst with
  | []    -> []
  | x::xs -> rev_list1 xs @ [x]

let is_palindrome lst = lst = (rev_list lst)

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

let () =
  print_endline "Hello, world!"
