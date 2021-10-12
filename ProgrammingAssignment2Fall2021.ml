

(* for testing *)
let rec between (n:int) (e:int): int list =
  let rec aux (n:int) (e:int) (ls:int list) = 
    if n <= e
    then aux n (e-1) (e :: ls)
    else ls
  in aux n e []

let rec print_list (ls: int list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | e::[] -> print_int e
    | e::l ->
      let _ = print_int e
      in let _ = print_string "; "
      in aux l
  in let _ = print_string "["
  in let _ = aux ls
  in         print_string "]"

let rec string_of_tuple_tuple_list (list: (int*int) list ): string =
  let rec aux list =
    match list with
      [] -> ""
    | (e,h)::[] ->
      "("^ (string_of_int e) ^"," ^ (string_of_int h) ^ ")"
    | (e,h)::l ->
      "(" ^ (string_of_int e) ^ "," ^ (string_of_int h) ^ "); " ^ (aux l)
  in "[ " ^ (aux list) ^ " ]"


(*
Write a safe_zip function that takes two lists of integers and combines them 
into a list of pairs of ints. If the two input list are of unequal lengths, 
return None. Your method should NOT be tail recursive.

For example,
safe_zip_int [1;2;3;5] [6;7;8;9] = Some [(1,6);(2,7);(3,8);(5,9)]
safe_zip_int [1] [2;4;6;8] = None
safe_zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
*)

let rec safe_zip (lsA: int list) (lsB: int list) : ((int * int) list) option =
  match (lsA, lsB) with
  | [], [] -> Some []
  | [], _ | _,[] -> None
  | (hdA::tlA),(hdB::tlB) -> match safe_zip tlA tlB with
    | None -> None
    | Some c -> Some ((hdA, hdB)::c)


(* Write a function that "unzips" a list of tuples into a tuple of two lists. This
function should be tail recursive.
   
For example:
unzip [(1, 2)] = [1], [2]
unzip [(1, 2); (3, 4); (5, 6); (7, 8)] = [1; 3; 5; 7], [2, 4, 6, 8]
unzip [] = []
*)

let unzip (ls : (int * int) list) : int list * int list = 
  let rec unzip' = fun t l1 l2 ->
    match t with
    | [] -> ([],[])
    | [(h1, h2)] -> (l1 @ [h1], l2 @ [h2] )
    | (h1, h2)::tail -> unzip' tail (l1 @ [h1] ) (l2 @ [h2])
  in unzip' ls [] []
  

(*
Write a function that produces the ith Pell number:
https://en.wikipedia.org/wiki/Pell_number
https://oeis.org/A000129
your function should be tail recursive. Errors in the result due to integer overflow
is expected for large inputs.

pell 0 = 0
pell 1 = 1
pell 7 = 169
pell 1000000  does not stack overflow
*)
  
let pell (i: int) : int = 
  let rec pell' j a b =
    match j with
    | 0-> a
    | 1 -> b
    | n -> pell' (n-1) b (2*b+a)
  in
  pell' i 0 1
  

(*
Infinite precision natural numbers can be represented as lists of ints between 0 and 9.

Write a function that takes an integer and represents it with a list of integers 
between 0 and 9 where the head is the least signifigant digit. If the input is 
zero or negative return an empty list.

toDec 1234 = [4; 3; 2; 1]
toDec 0 = []
toDec (-1234) = []
*)

(* Hint use
   mod 10
   / 10
*)
  

let rec toDec (i : int) : int list =
  let rec aux acc i = 
    if i <= 0 then acc 
    else aux (acc @ [i mod 10]) (i/10)
  in
  aux [] i

(*
Write a function that sums 2 natural numbers as represented by a list of integers between 0 and 9 
where the head is the least signifigant digit. Your function should be tail recursive

sum [4; 3; 2; 1] [1;0;1] = [5; 3; 3; 1]
sum [1] [9;9;9] = [0; 0; 0; 1]
sum [] [] = []
sum (nines 1000000) [1] does not stack overflow, when (nines 1000000) provides a list of 1000000 9s
*)

let rec sum (a : int list) (b : int list): int list = 
  let rec sum' = fun x y current carry ->
  match (x, y) with
    | ([],[]) -> if carry=1 then current@[1] else current
    | ([],h::t) -> if h+carry >= 10 then sum' [] t (current @ [(((h)+carry) mod 10)]) 1 else sum' [] t (current @ [h+carry]) 0
    | (h::t,[]) -> if h+carry >= 10 then sum' t [] (current @ [(((h)+carry) mod 10)]) 1 else sum' t [] (current @ [h+carry]) 0
    | (h1::t1,h2::t2) -> if h1+h2+carry >= 10 then sum' t1 t2 (current @ [(((h1+h2)+carry) mod 10)]) 1 else sum' t1 t2 (current @ [h1+h2+carry]) 0
  in sum' a b [] 0


(* Write an infinite precision version of the Pell numbers from before

pell2 0 = []
pell2 1 = [1]
pell2 7 = [9; 6; 1]
pell2 50 = [2; 2; 5; 3; 5; 1; 4; 2; 9; 2; 4; 6; 2; 5; 7; 6; 6; 8; 4]

*)

let pell2 (i: int) : int list = 
  let rec pell2' = fun j a b ->
    match j with
    | 0 -> []
    | 1 -> b
    | n -> pell2' (n-1) b (sum a (sum b b))
  in pell2' i [0] [1]
