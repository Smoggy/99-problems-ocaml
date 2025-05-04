(* Tail of a List *)

let rec last_element list =
  match list with
    | [] -> None
    | [el] -> Some el
    | _ :: tail -> last_element tail

(* Last Two Elements of a List *)
let rec last_two_elements list = 
  match list with
    | [] | [_] -> None
    | [el1; el2] -> Some (el1, el2)
    | _ :: tail -> last_two_elements tail


(* N'th Element of a List *)
let at index list =
  let rec impl counter = function
      | [] -> None
      | head :: tail ->
        match counter with
          | cur when cur = index -> Some head
          | _ -> impl (counter+1) tail
  in
  impl 0 list

(* N'th Element of a List *)
let length list =
  let rec impl c list =  
    match list with
      | [] -> c
      | _ :: tail -> impl (c + 1) tail
  in
  impl 0 list
  
(* Reverse a List *)
let reverse list =
  let rec impl acc = function
      | [] -> acc
      | h :: t -> impl (h::acc) t in
  impl [] list 

(* Palindrome *)
let is_palindrome list =
  list = reverse list

(* Flatten a List *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten (list: 'a node list) : 'a list  =
  let rec impl acc = function
      | [] -> acc
      | One x :: t -> impl (x :: acc) t
      | Many h :: t ->  impl (impl acc h) t
  in
  List.rev (impl [] list)

(*Eliminate Duplicates*)

let compress_tail l: 'a list =
  let rec impl acc = function
    | [] -> acc
    | h1 :: (h2 :: _ as tail) -> if h1 = h2 then impl acc tail else impl (h1 :: acc) tail
    | h1 :: tail -> impl (h1 :: acc) tail
  in
  impl [] l |> List.rev

let rec compress = function
  | h1 :: (h2 :: _ as tail) -> if h1 = h2 then compress tail else h1 :: compress tail
  | rest -> rest

(* Pack Consecutive Duplicates *)

let pack (lst: string list) =
  let rec impl acc cur = function
    | [] -> acc
    | [h] -> (h :: cur) :: acc
    | h1 :: (h2 :: _ as tail) -> 
      if h1 = h2 then impl acc (h1 :: cur) tail 
      else  impl ((h1 :: cur) :: acc) [] tail
  in
  List.rev (impl [] [] lst)

(* Run-Length Encoding *)

let encode_simple lst =
  let rec impl acc cur = function
    | [] -> acc
    | [h] -> (cur + 1, h) :: acc
    | h1 :: (h2 :: _ as tail) -> 
      if h1 = h2 then impl acc (cur + 1) tail 
      else  impl ((cur + 1, h1) :: acc) 0 tail
  in
  List.rev (impl [] 0 lst)

(* Modified Run-Length Encoding *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_modified lst =
  let map_rle c l = if c = 1 then One l else Many (c, l)
  in
  let rec impl acc cur = function
    | [] -> acc
    | [h] -> (map_rle (cur + 1) h) :: acc
    | h1 :: (h2 :: _ as tail) -> 
      if h1 = h2 then impl acc (cur + 1) tail 
      else  impl ((map_rle (cur + 1) h1) :: acc) 0 tail
  in
  List.rev (impl [] 0 lst)

(* Decode a Run-Length Encoded List *)

let decode (lst: 'a rle list) = 
  let rec fill_list acc n c = if n = 0 then acc else fill_list (c :: acc) (n - 1) c
  in
  let rec impl acc (l: 'a rle list) =
    match l with 
      | [] -> acc
      | One x :: t -> impl (x :: acc) t
      | Many (n, x) :: t -> impl (fill_list acc n x) t
  in
  List.rev (impl [] lst)

(* Run-Length Encoding of a List (Direct Solution) *)

let encode_direct lst =
  let rle_of_tuple n x = if n = 1 then One x else Many (n, x) in 
  let rec impl acc cur lst =
    match lst with 
      | [] -> acc
      | [e] -> (rle_of_tuple (cur + 1) e) :: acc
      | h1 :: (h2 :: _ as tail) -> 
          if h1 = h2 then
            impl acc (cur + 1) tail else
            impl ((rle_of_tuple (cur + 1) h1) :: acc) 0 tail
  in
  List.rev (impl [] 0 lst) 

(* Duplicate the Elements of a List *)

let dublicate lst = 
  let rec impl acc lst = 
    match lst with
    | [] -> acc
    | h :: t -> impl (h :: h :: acc) t
  in
  List.rev (impl [] lst) 

(* Replicate the Elements of a List a Given Number of Times *)
let replicate lst n =
  let rec fill acc c n = if n = 0 then acc else fill (c :: acc) c (n - 1) in
  let rec impl acc lst = 
    match lst with
    | [] -> acc
    | h :: t -> impl (fill acc h n) t
  in impl [] (List.rev lst)

(* Drop Every N'th Element From a List *)

let drop lst n =
  let rec impl acc cur lst =
    match lst with
      | [] -> acc
      | h :: t -> if cur = n then impl acc 1 t else impl (h :: acc) (cur + 1) t
  in
  List.rev (impl [] 1 lst)

(* Split a List Into Two Parts; The Length of the First Part Is Given *)

let split lst n = 
  let rec impl acc c lst =
    match lst with
    | [] -> List.rev acc, []
    | h :: t as rest -> if c = 0 then List.rev acc, rest else  impl (h :: acc) (c - 1) t
  in
  impl [] n lst

(* Extract a Slice From a List *)

let slice lst i k = 
  let rec impl acc lst c =
    match lst with
    | [] -> List.rev acc
    | h :: t -> if c > k then List.rev acc 
      else if c < i then impl acc t (c + 1) 
      else impl (h :: acc) t (c + 1)
  in
  impl [] lst 0

(* Rotate a List N Places to the Left *)

let rotate lst n =
  let rec impl acc c = function
    | [] -> List.rev acc
    | h :: t as rest -> if c = 0 then rest @ List.rev acc else impl (h ::acc) (c - 1) t
  in
  let len = List.length lst in
    if len = 0 then lst else impl [] (n mod len) lst
(* Remove the K'th Element From a List *)
let remove_at n lst =
  let rec impl acc c = function 
    | [] -> List.rev acc
    | h :: t -> if c = n then (List.rev acc) @ t else impl (h :: acc) (c + 1) t
  in
    impl [] 0 lst

let rec remove_at_simple n = function
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at_simple (n - 1) t

(* Insert an Element at a Given Position Into a List *)

let insert_at el i lst = 
  let rec impl acc c = function
    | [] -> List.rev (el :: acc)
    | h :: t as rest -> if c = i then (List.rev (el :: acc)) @ rest else impl (h :: acc) (c + 1) t
  in
    impl [] 0 lst

let rec insert_at_simple el i = function
  | [] -> [el]
  | h :: t as rest -> if i = 0 then el :: rest else h :: insert_at_simple el (i - 1) t

(* Create a List Containing All Integers Within a Given Range *)

let rec range s e =
  let op = if s > e then (-) else (+) in
  if s = e then [s] else s :: range (op s 1) e 

(* Extract a Given Number of Randomly Distinct Selected Elements From a List *)

let rand_select lst n =
  Random.init 0; (* for testing simplicity*)
  let rec choose_element_at acc i = function
    | [] -> failwith "out of bound"
    | h :: t -> if i = 0 then (h, acc @ t) else choose_element_at (h :: acc) (i - 1) t
  in
  let rec impl n acc lst len = 
    if n = 0 then acc else
      let chosen, rest = choose_element_at [] (Random.int len) lst in
      impl (n - 1) (chosen :: acc) rest (len - 1)
  in
    impl n [] lst (List.length lst)

(* Lotto: Draw N Different Random Numbers From the Set 1..M *)

let lotto_select n m =
  Random.init 0;  (* for testing simplicity*)
  let rec get_rand lst =
    let rand = Random.int m in
    if List.mem rand lst then get_rand lst else rand
  in
  let rec impl c acc = 
    if c = 0 then acc else impl (c - 1) (get_rand acc :: acc)
  in
    impl n []

let lotto_select_simple n m = 
  rand_select (range 1 m) n

(* Generate a Random Permutation of the Elements of a List *)

let permutation lst = 
  rand_select lst (List.length lst)

(* Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List *)

let rec combinations n lst = 
  if n <= 0 then [[]]
  else match lst with
    | [] -> []
    | h :: t ->
      let current_el_combinations = List.map (fun l -> h :: l) (combinations (n - 1) t) in 
      let rest = combinations n t in
      current_el_combinations @ rest
