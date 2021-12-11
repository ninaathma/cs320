(*
Honor code comes here:

First Name: Nina 
Last Name: Athma
BU ID: U51803274

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)

(* the type of a polymorphic tree *)
type 'a tree =
  | Leaf of 'a 
  | Node of 'a tree * 'a tree

(*
write a map function for trees:

For example,
map_tree (fun x -> x+1) (Node (Leaf 1, Leaf 2)) =  (Node (Leaf 2, Leaf 3))
map_tree (fun _ -> 0)  (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
                       (Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Leaf 0    )))
*)

let rec map_tree (f: 'a -> 'b) (tree: 'a tree) =
  match tree with
  | Leaf n -> Leaf (f n)
  | Node(l,r) -> Node((map_tree f l), (map_tree f r));;

  (*
write a fold function for trees:

For example,
fold_tree ( * ) (fun x -> x) (Node (Leaf 3, Leaf 2)) = 6
fold_tree (+) (fun _ -> 1) (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) = 7
*)

let rec fold_tree (node: 'b -> 'b -> 'b)  (leaf: 'a -> 'b)  (tree: 'a tree): 'b  = failwith "unimplemented"

let rec fold_tree (node: 'b -> 'b -> 'b)  (leaf: 'a -> 'b)  (tree: 'a tree): 'b  =
  match tree with
    | Leaf v -> leaf v
    | Node(l,r) -> node (fold_tree node leaf l) (fold_tree node leaf r)

(*
sum the contents of an int tree

For example,
sum_ints (Node (Leaf 1, Leaf 2)) = 3
*)
let rec sum_ints (tree: int tree): int  = failwith "unimplemented"

let rec sum_ints (tree: int tree): int =
  match tree with
  | Leaf v -> v
  | Node (l, r) -> sum_ints l + sum_ints r

(*
find the size of the tree

For example,
tree_size (Leaf 1) = 1
tree_size (Node (Leaf 1, Leaf 2)) = 3
*)

let rec tree_size (tree: 'a tree): int  = 
  match tree with
  | Leaf l -> 1
  | Node (l, r) -> tree_size l + tree_size r + 1


(*
find the height of the tree

For example,
tree_height (Leaf 2) = 1
tree_height (Node ((Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))), Leaf 2)) = 5
*)
let rec tree_height (tree: 'a tree): int  = 
  match tree with
  | Leaf n -> 1
  | Node (l, r) -> if tree_height l > tree_height r then (tree_height l)+1 else (tree_height r)+1

(* let rec tree_height (t: 'a tree) : int =
  match t with
  | Empty -> 0
  | Node (l, r) -> 1 + max (tree_height l) (tree_height r));; *)
  
(*
write a function that takes a predicate on trees and retuns true if any subtree satisfies that predicate

For example,
tree_contains (Node (Leaf 1, Leaf 2)) (fun x -> match x with Leaf 2 -> true | _ -> false) = true
tree_contains (Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))) (fun x -> tree_height x > 2) = true
*)
let rec tree_contains (tree: 'a tree) (look_for: 'a tree -> bool): bool  = 
  if look_for tree then true else
    match tree with 
    | Leaf n -> false
    | Node(l,r) -> tree_contains l (look_for) || tree_contains r (look_for)

(*
write a function that shows bool trees :

For example,
show_bool_tree (Leaf true) ="true"
show_bool_tree (Node (Leaf true, Leaf false)) = "(true^false)" 
show_bool_tree  (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
    "((true^(true^false))^((true^(true^false))^false))" 
*)

let rec show_bool_tree (tree: bool tree) : string =
  match tree with
  | Leaf n -> if n then "true" else "false"
  | Node (l, r) -> "("^(show_bool_tree l)^"^"^(show_bool_tree r)^")"

(* standard functions to convert between string and char list *)
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)

(*
write a function that reads bool trees :
for all (finite) t : bool trees.
read_bool_tree t = Some (show_bool_tree t)

For example,
read_bool_tree "true" = Some (Leaf true)
read_bool_tree "false" = Some (Leaf false)
read_bool_tree "tralse" = None
read_bool_tree "(true^false)" = Some (Node (Leaf true, Leaf false))
read_bool_tree "((true^(true^false))^((true^(true^false))^false))" =
Some
 (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false)))
*)

(* Hint 
write a helper function 
read_bool_prefix : (char list) -> ((bool * (char list)) option) 

such that
read_bool_prefix (explode "true???")       = Some (true, ['?'; '?'; '?'])
read_bool_prefix (explode "false123")      = Some (false, ['1'; '2'; '3'])
read_bool_prefix (explode "antythingales") = None
read_bool_prefix []                        = None

write a helper function 
read_bool_tree_prefix (char list) -> ((bool tree * (char list)) option) 

such that
read_bool_tree_prefix [] = None
read_bool_tree_prefix (explode "true???") = Some (Leaf true, ['?'; '?'; '?'])
read_bool_tree_prefix (explode "(true^false)124") = Some (Node (Leaf true, Leaf false), ['1'; '2'; '4'])
read_bool_tree_prefix (explode "(true^(true^false))aaa") = Some (Node (Leaf true, Node (Leaf true, Leaf false)), ['a'; 'a'; 'a'])
read_bool_tree_prefix (explode "(true^(true^fa se))aaa") = None
*)

let rec read_bool_tree (tree: string) : ((bool tree) option) = failwith "unimplimented"


(*
write a fubction that checks that parenthisis are balnaced:
Parenthisis are balenced if there are no parenthises
Parenthisis are balenced if ( and )  enclose a balenced parenthises
Parenthisis are balenced if balenced parenthises are ajacent to a balenced parenthisis

For example,
matching_parens "" = true
matching_parens "((((((((((()))))))))))" = true
matching_parens "()()()()()()" = true
matching_parens "(()())" = true
matching_parens "())(()" = false
*)

(* Hint 
write mutually recursive functions 
matching_paren_prefix : (char list) -> ((char list) option)
matching_parens_prefix : (char list) -> ((char list) option)

the and keyword allows mutual recursion
let rec matching_paren_prefix (ls: char list) : ((char list) option) = failwith "unimplemented"
and matching_parens_prefix  (ls: char list) : ((char list) option) = failwith "unimplemented"

such that
matching_paren_prefix [] = None
matching_paren_prefix (explode "(???") = None
matching_paren_prefix (explode "()???") = Some ['?'; '?'; '?']
matching_paren_prefix (explode "(((())))123") = Some ['1'; '2'; '3']
matching_paren_prefix (explode "()()()") = Some ['('; ')'; '('; ')']
matching_paren_prefix (explode "(()()())abc") = Some ['a'; 'b'; 'c']

matching_parens_prefix [] = Some []
matching_parens_prefix (explode "()()()") = Some []
matching_parens_prefix (explode "()())))") = Some [')'; ')'; ')']
matching_parens_prefix (explode ")aa") = Some [')'; 'a'; 'a']
*)


let rec matching_parents (tree: string) : bool= failwith "unimplimented"
let rec matching_parens (tree: string) : bool  =
  let rec aux s left right last = 
     match s with
        | [] -> if left=right then true else false
        | hd::tl -> if (hd = ')' && right+1 > left) then false 
        else if hd = '(' then aux (tl) (left + 1) (right) (hd) 
        else aux (tl) (left) (right+1) (hd)

  in aux (explode (tree)) (0) (0) '('