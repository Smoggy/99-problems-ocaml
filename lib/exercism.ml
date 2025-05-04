
(* Valid Brackets *)
let check_pair closeBracket openBracket =
  match closeBracket with
  | ')' -> openBracket = '('
  | ']' -> openBracket = '['
  | '}' -> openBracket = '{'
  | _ -> false

let are_balanced str =
  let rec impl list opened =
    match list with
    | [] -> opened = []
    | h :: t  when h = '(' || h = '{' || h = '[' -> impl t (h :: opened)
    | h :: t when h = ')' || h = '}' || h = ']' ->
      (match opened with
        | [] -> false 
        | h1 :: t1 -> 
            if check_pair h1 h 
            then impl t t1 
            else false)
    | _ :: t -> impl t opened  in
  impl (str |> String.to_seq |> List.of_seq) []

let a = are_balanced "[]"

(* Difference of squares*)

let rec pow base exp =
  match exp with
  | 0 -> 1
  | _ -> base * pow base exp - 1

let square_of_sum number = 
  let rec impl n acc=
    match n with
    | 0 -> acc
    | _ -> impl (n -1) (acc + n)
  in
  pow (impl number 0) 2

let sum_of_squares number =
  let rec impl n acc =
    match n with
    | 0 -> acc
    | _ -> impl (n-1) (acc + pow n 2)
  in
  impl number 0

let difference_of_squares number =
  square_of_sum(number) - sum_of_squares(number)

(* Hamming *)

type nucleotide = A | C | G | T

let hamming_distance a b =
  match (a, b) with
  | [], _ -> Error "left strand must not be empty"
  | _, [] -> Error "right strand must not be empty"
  | _ when List.length a <> List.length b -> Error "left and right strands must be of equal length"
  | _ -> Ok (List.combine a b |> List.filter (fun (el1, el2) -> el1 <> el2) |> List.length)

(* Nucleotide Count *)

module NucleotideCountModule = struct
  open Base

  let empty = Map.empty (module Char)

  let is_nucleotide = function
    | 'A' | 'C' | 'G' | 'T' -> true
    | _ -> false

  let count_nucliotide s c = 
    if not (is_nucleotide c) then Error "INVALID"
    else match String.find s ~f:(fun x -> not (is_nucleotide x)) with
      | Some _ -> Error "INVALID"
      | None -> Ok (String.count s ~f:(Char.equal c))

  let count_nucleotides s =
    match String.find s ~f:(fun x -> not (is_nucleotide x)) with
    | Some c -> Error c
    | None ->
      let map =
        String.fold s ~init:empty ~f:(fun count c ->
          Map.update count c ~f:(function
            | Some n -> n + 1
            | None -> 1))
      in
      Ok map
end

(* Anagrams *)

let rec anagrams target lst =
  let sort str = 
    str |> String.lowercase_ascii |> String.to_seq |> List.of_seq |> List.sort Char.compare
  in 
  let lowercase_target = sort target in
    match lst with
    | [] -> []
    | h :: t -> if String.lowercase_ascii h = String.lowercase_ascii target then anagrams target t 
      else if lowercase_target = sort h then h :: anagrams target t
      else anagrams target t

(* Pangram *)

module CharSet = Set.Make(Char)

let is_pangram str =
  str |> String.lowercase_ascii
    |> String.to_seq
    |> CharSet.of_seq
    |> CharSet.subset (CharSet.of_list ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'])