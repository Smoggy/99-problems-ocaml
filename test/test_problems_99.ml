open Alcotest
open Mylib.Problems_99

let test_last_element () =
  Alcotest.(check (option string)) "last of list" (Some "c") (last_element ["a"; "b"; "c"]);
  Alcotest.(check (option string)) "empty list" None (last_element [])

let test_last_two_elements () =
  Alcotest.(check (option (pair string string))) "last two elements in list" (Some ("c", "d")) (last_two_elements ["a"; "b"; "c"; "d"]);
  Alcotest.(check (option (pair string string))) "last two elements in list" None (last_two_elements ["a"]);
  Alcotest.(check (option (pair string string))) "empty list" None (last_two_elements [])

let test_at () =
  Alcotest.(check (option string)) "index is less then list length" (Some "c") (at 2 ["a"; "b"; "c"]);
  Alcotest.(check (option string)) "index is bigger then list length" None (at 3 ["a"])

let test_length () =
  Alcotest.(check int) "list with elements" 3 (length ["a"; "b"; "c"]);
  Alcotest.(check int) "empty list" 0 (length [])

let test_reverse () =
  Alcotest.(check (list string)) "ordinary list" (["c"; "b"; "a"]) (reverse ["a"; "b"; "c"])

let test_is_palindrome () =
  Alcotest.(check bool) "palindrome list" true (is_palindrome ["a"; "b"; "b"; "a"]);
  Alcotest.(check bool) "not palindrome list" false (is_palindrome ["a"; "b"])

let test_flatten () =
  Alcotest.(check (list string)) "list with one and many" (["a"; "b"; "c"; "d"; "e"]) (flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]]);
  Alcotest.(check (list string)) "empty list" ([]) (flatten [])

let test_compress () =
  Alcotest.(check (list string)) "list with multiple consecutive duplicates" (["a"; "b"; "c"; "a"; "d"; "e"]) (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);
  Alcotest.(check (list string)) "list with no duplicates"  (["a"; "b"; "c"; "a"; "d"; "e"]) (compress ["a"; "b"; "c"; "a"; "d"; "e"]);
  Alcotest.(check (list string)) "list with one element" (["a"]) (compress_tail ["a"]);
  Alcotest.(check (list string)) "empty list" ([]) (compress [])

let test_pack () =
  Alcotest.(check (list (list string))) "list with multiple consecutive duplicates" ([["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]) (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]);
  Alcotest.(check (list (list string))) "list with one element" ([["a"]]) (pack ["a"]);
  Alcotest.(check (list (list string))) "empty list" ([]) (pack [])

let test_encode_simple () =
  Alcotest.(check (list (pair int  string))) "list with multiple consecutive duplicates" ([(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]) (encode_simple ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);
  Alcotest.(check (list (pair int  string))) "empty list" ([]) (encode_simple [])

let pp_rle fmt = function
  | One x -> Format.fprintf fmt "One %S" x
  | Many (n, x) -> Format.fprintf fmt "Many (%d, %S)" n x

let string_rle_testable =
  Alcotest.testable pp_rle (=)

let test_encode_modified () = 
  Alcotest.(check (list string_rle_testable)) "list with multiple consecutive duplicates" ([Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]) (encode_modified ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);
  Alcotest.(check (list string_rle_testable)) "empty list" ([]) (encode_modified [])

let test_decode () =
  Alcotest.(check (list string)) "list with multiple elements" (["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);
  Alcotest.(check (list string)) "empty list" ([]) (decode [])

let test_encode_direct () = 
  Alcotest.(check (list string_rle_testable)) "list with multiple consecutive duplicates" ([Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]) (encode_direct ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);
  Alcotest.(check (list string_rle_testable)) "empty list" ([]) (encode_direct [])

let test_dublicate () =
  Alcotest.(check (list string)) "list with multiple elements" (["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]) (dublicate ["a"; "b"; "c"; "c"; "d"]);
  Alcotest.(check (list string)) "empty list" ([]) (dublicate [])

let test_replicate () =
  Alcotest.(check (list string)) "list with multiple elements" (["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]) (replicate ["a"; "b"; "c"] 3);
  Alcotest.(check (list string)) "empty list" ([]) (replicate [] 3)

let test_drop () =
  Alcotest.(check (list string)) "list with multiple elements" (["a"; "b"; "d"; "e"; "g"; "h"; "j"]) (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3);
  Alcotest.(check (list string)) "n greater than number of elements" (["a"; "b"]) (drop ["a"; "b"] 3);
  Alcotest.(check (list string)) "empty list" ([]) (drop [] 3)

let test_split () =
  Alcotest.(check (pair (list string) (list string))) "list with multiple elements" (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]) (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3);
  Alcotest.(check (pair (list string) (list string))) "n greater than number of elements" (["a"; "b"; "c"; "d"], []) (split ["a"; "b"; "c"; "d"] 53);
  Alcotest.(check (pair (list string) (list string))) "empty list" ([], []) (split [] 3)

let test_slice () =
  Alcotest.(check (list string)) "list with multiple elements" (["c"; "d"; "e"; "f"; "g"]) (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6);
  Alcotest.(check (list string)) "empty list" ([]) (slice [] 1 2)

let test_rotate() = 
Alcotest.(check (list string)) "list with multiple elements" (["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]) (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3);
Alcotest.(check (list string)) "empty list" ([]) (rotate [] 3)

let test_remove_at() =
  Alcotest.(check (list string)) "list with multiple elements and correct index" (["a"; "c"; "d"]) (remove_at 1 ["a"; "b"; "c"; "d"]);
  Alcotest.(check (list string)) "list with multiple elements and incorrect index" (["a"; "b"; "c"; "d"]) (remove_at 6 ["a"; "b"; "c"; "d"]);
  Alcotest.(check (list string)) "empty list" ([]) (remove_at 3 [])

let test_insert_at() =
  Alcotest.(check (list string)) "list with multiple elements and correct index" (["a"; "alfa"; "b"; "c"; "d"]) (insert_at "alfa" 1 ["a"; "b"; "c"; "d"]);
  Alcotest.(check (list string)) "list with multiple elements and incorrect index" (["a"; "b"; "c"; "d"; "alfa"]) (insert_at "alfa" 6 ["a"; "b"; "c"; "d"])

let test_range() =
  Alcotest.(check (list int)) "ascending order" ([4; 5; 6; 7; 8; 9]) (range 4 9);
  Alcotest.(check (list int)) "descending order" ([9; 8; 7; 6; 5; 4]) (range 9 4)

let test_rand_select() =
  Alcotest.(check (list string)) "list with multiple elements" (["e"; "c"; "g"]) (rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3);
  Alcotest.check_raises "empty list should raise" 
    (Invalid_argument "Random.int")
    (fun () -> ignore (rand_select [] 1))

let test_lotto_select() =
  Alcotest.(check (list int)) "list with 6 numbers" ([18; 23; 34; 19; 3; 28]) (lotto_select 6 49)

let test_permutation() = 
  Alcotest.(check (list string)) "list with multiple elements" (["e"; "d"; "c"; "b"; "f"; "a"]) (permutation ["a"; "b"; "c"; "d"; "e"; "f"])

let test_combinations() =
  Alcotest.(check (list (list string))) "list with multiple elements"  ([["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]) (combinations 2 ["a"; "b"; "c"; "d"])

  let () = 
  Alcotest.run "99 Problems Tests" [
    "last_element", [test_case "Test last element" `Quick test_last_element];
    "last_two_elements", [test_case "Test last two elements" `Quick test_last_two_elements];
    "at", [test_case "Test N'th Element of a List" `Quick test_at];
    "length", [test_case "Test length of the list" `Quick test_length];
    "reverse", [test_case "Test reverse list" `Quick test_reverse];
    "palindrome", [test_case "Test is palindrome" `Quick test_is_palindrome];
    "flatten", [test_case "Test flatten" `Quick test_flatten];
    "compress", [test_case "Test comporess" `Quick test_compress];
    "pack", [test_case "Test pack" `Quick test_pack];
    "simple_encode", [test_case "Test simple encode" `Quick test_encode_simple];
    "encode_modified", [test_case "Test encode modified" `Quick test_encode_modified];
    "decode", [test_case "Test decode" `Quick test_decode];
    "encode_direct", [test_case "Test encode direct" `Quick test_encode_direct];
    "duplicate", [test_case "Test duplicate" `Quick test_dublicate];
    "replicate", [test_case "Test replicate" `Quick test_replicate];
    "drop", [test_case "Test drop" `Quick test_drop];
    "split", [test_case "Test split" `Quick test_split];
    "slice", [test_case "Test slice" `Quick test_slice];
    "rotate", [test_case "Test rotate" `Quick test_rotate];
    "remove_at", [test_case "Test remove_at" `Quick test_remove_at];
    "insert_at", [test_case "Test insert_at" `Quick test_insert_at];
    "range", [test_case "Test range" `Quick test_range];
    "rand_select", [test_case "Test random select" `Quick test_rand_select];
    "lotto_select", [test_case "Test lotto_select" `Quick test_lotto_select];
    "permutation", [test_case "Test permutation" `Quick test_permutation];
    "combinations", [test_case "Test combinations" `Quick test_combinations]
  ]