let rec rev_list1 lst = match lst with
  | []    -> []
  | x::xs -> rev_list1 xs @ [x]

let rec rev_list ( lst : 'a list ) : 'a list =
  let rec append x xs = match xs with
    | []           -> [x]
    | head :: tail -> head :: append x tail
  in match lst with
     | [] -> []
     | head :: tail -> append head (rev_list tail)

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

let () =
  print_endline "Hello, world!"
