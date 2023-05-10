(* Creates a linearly spaced list of floats*)
let rec seq x xs sp =
	if x > xs +. sp then []
	else x :: seq (x +. sp) xs sp

(* folds the list with the given function but stores the intermediate results. Similar to the Haskell function scanl*)
let rec scan_left f acc lst =
 	match lst with
		| [] -> [acc]
		| x :: xs -> let acc' = f acc x in
      			acc :: scan_left f acc' xs

(* defining an infix operator to index lists*)
let ($) lst n = List.nth lst n
