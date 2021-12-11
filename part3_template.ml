(* part3 template *)
type name = string

type term =
  (* lambda calculus *)
  | Name  of name
  | Fun   of name * name * term
  | App   of term * term
  (* extensions *)
  | Ifgz  of term * term * term
  | LetIn of name * term * term
  | Unit
  | Int   of int
  | Add   of term * term
  | Sub   of term * term
  | Mul   of term * term
  | Div   of term * term
  | Trace of term

type value =
  | IntVal of int
  | FunVal of name * name * term * env
  | UnitVal

and env = (string * value) list

type error =
  | TypeError
  | DivByZeroError
  | UnboundError

type result =
  | Value of value
  | Error of error
(* parser for term language *)

(* util functions *)

let is_lower_case c =
  'a' <= c && c <= 'z'

let is_upper_case c =
  'A' <= c && c <= 'Z'

let is_alpha c =
  is_lower_case c || is_upper_case c

let is_digit c =
  '0' <= c && c <= '9'

let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c

let is_blank c =
  String.contains " \012\n\r\t" c

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
    match p ls with
    | Some (a, ls) -> q a ls
    | None -> None

let (>>=) = bind
let (let*) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None

let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let digit : char parser =
  satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()
  
(* end of parser combinators *)

let reserved = [
  "fun";
  "if";
  "then";
  "else";
  "let";
  "rec";
  "in";
  "trace";
  "main";
]

let name : string parser =
  let* xs1 = many1 (satisfy (fun c -> is_alpha c || c = '_')) in
  let* xs2 = 
    many (satisfy (fun c ->
      is_alphanum c ||
      (c = '_') ||
      (c = '\'')))
  in
  let s = (implode xs1) ^ (implode xs2) in
  if List.exists (fun x -> x = s) reserved
  then fail
  else pure s << ws

let name_parser () =
  let* n = name in
  pure (Name n)

let unit_parser () =
  let* _ = keyword "()" in
  pure (Unit)

let int_parser () =
  let* n = natural in
  pure (Int n) << ws

let rec term_parser0 () =
  let* _ = pure () in
  (name_parser ()) <|>
  (unit_parser ()) <|>
  (int_parser ()) <|>
  (keyword "(" >> term_parser () << keyword ")")

and term_parser1 () =
  let* es = many1 (term_parser0 ()) in
  match es with
  | e :: es ->
    pure (List.fold_left (fun acc e -> App (acc, e)) e es)
  | _ -> fail

and term_parser2 () =
  let* e = term_parser1 () in
  let opr () = 
    (let* _ = keyword "*" in
    let* e = term_parser1 () in
    pure ((fun e1 e2 -> Mul (e1, e2)), e))
    <|>
    (let* _ = keyword "/" in
    let* e = term_parser1 () in
    pure ((fun e1 e2 -> Div (e1, e2)), e))
  in
  let* es = many (opr ()) in
  pure (List.fold_left (fun acc (f, e) -> f acc e) e es)

and term_parser3 () =
  let* e = term_parser2 () in
  let opr () = 
    (let* _ = keyword "+" in
    let* e = term_parser2 () in
    pure ((fun e1 e2 -> Add (e1, e2)), e))
    <|>
    (let* _ = keyword "-" in
    let* e = term_parser2 () in
    pure ((fun e1 e2 -> Sub (e1, e2)), e))
  in
  let* es = many (opr ()) in
  pure (List.fold_left (fun acc (f, e) -> f acc e) e es)

and term_parser () = 
  let* _ = pure () in
  (keyword "trace" >> term_parser3 () >|= (fun e -> Trace e)) <|>
  (fun_parser ()) <|>
  (iflz_parser ()) <|>
  (letrec_parser ()) <|>
  (letin_parser ()) <|>
  (term_parser3 ())

and fun_parser () =
  let* _ = keyword "fun" in
  let* a = name in
  let* _ = keyword "->" in
  let* e = term_parser () in
  pure (Fun ("_", a, e))

and iflz_parser () =
  let* _ = keyword "if" in
  let* cond = term_parser () in
  let* _ = keyword "then" in
  let* e1 = term_parser () in
  let* _ = keyword "else" in
  let* e2 = term_parser () in
  pure (Ifgz (cond, e1, e2))

and letin_parser () =
  let* _ = keyword "let" in
  let* n = name in
  let* _ = keyword "=" in
  let* e1 = term_parser () in
  let* _ = keyword "in" in
  let* e2 = term_parser () in
  pure (LetIn (n, e1, e2))

and letrec_parser () =
  let* _ = keyword "let" in
  let* _ = keyword "rec" in
  let* n = name in
  let* args = many1 name in
  let* _ = keyword "=" in
  let* e1 = term_parser () in
  let (e1, _) =
    List.fold_right
      (fun arg (acc, len) ->
        let fn = if len = 1 then n else "_" in
        (Fun (fn, arg, acc), len - 1))
      args (e1, List.length args)
  in
  let* _ = keyword "in" in
  let* e2 = term_parser () in
  pure (LetIn (n, e1, e2))

let string_of_value v =
  match v with
  | IntVal n -> string_of_int n
  | FunVal _ -> "<fun>"
  | UnitVal  -> "()"

let string_of_error err =
  match err with
  | TypeError      -> "TypeError"
  | DivByZeroError -> "DivByZeroError"
  | UnboundError   -> "UnboundError"

let string_of_result res =
  match res with
  | Value v -> string_of_value v
  | Error err -> string_of_error err

let string_of_log log = 
  let log =
    (List.fold_left 
      (fun acc log -> 
        if acc = ""
        then log
        else log ^ "; " ^ acc )
      "" log)
  in
  "[" ^ log ^ "]"

let string_of_env env =
  (List.fold_left
    (fun acc (s, v) ->
      if acc = "" 
      then s ^ ":=" ^ (string_of_value v)
      else s ^ ":=" ^ (string_of_value v) ^ "; " ^ acc))
    "" env
  
let rec string_of_term t =
  match t with
  | Name x -> x
  | Fun (fn, arg, e) -> 
    "Fun (" ^ fn ^ ", " ^ arg ^ ", " ^ (string_of_term e) ^ ")"
  | App (e1, e2) -> 
    "App (" ^ (string_of_term e1) ^ ", " ^ (string_of_term e2) ^ ")"
  (* extensions *)
  | Ifgz (cond, e1, e2) ->
    let cond = string_of_term cond in
    let e1 = string_of_term e1 in
    let e2 = string_of_term e2 in
    "Ifgz (" ^ cond ^ ", " ^ e1 ^ ", " ^ e2 ^ ")"
  | LetIn (x, e1, e2) ->
    let e1 = string_of_term e1 in
    let e2 = string_of_term e2 in
    "LetIn (" ^ x ^ ", " ^ e1 ^ ", " ^ e2 ^ ")"
  | Unit -> "Unit"
  | Int n -> string_of_int n
  | Add (e1, e2) ->
    let e1 = string_of_term e1 in
    let e2 = string_of_term e2 in
    "Add (" ^ e1 ^ ", " ^ e2 ^ ")"
  | Sub (e1, e2) ->
    let e1 = string_of_term e1 in
    let e2 = string_of_term e2 in
    "Sub (" ^ e1 ^ ", " ^ e2 ^ ")"
  | Mul (e1, e2) ->
    let e1 = string_of_term e1 in
    let e2 = string_of_term e2 in
    "Mul (" ^ e1 ^ ", " ^ e2 ^ ")"
  | Div (e1, e2) ->
    let e1 = string_of_term e1 in
    let e2 = string_of_term e2 in
    "Div (" ^ e1 ^ ", " ^ e2 ^ ")"
  | Trace e ->
    "Trace (" ^ (string_of_term e) ^ ")"
    
(* Parse programs written in the term language. *)
let parse_prog (s : string) : (term * char list) option = 
  parse (ws >> term_parser ()) s
  
(* Compiles a program written in the Part3 Term Language to Part2 Stack Language.
 * Required by the autograder, do not change its type. *)

let rec compile (t: term) : string =
  match t with
    | Name n -> "Push "^n^" Lookup"
    | Int i -> "Push "^(string_of_int i)
    | Unit  -> "Push ()"
    | Ifgz (term1, term2, term3) -> compile term1^" If Begin "^compile term2^" End Else Begin "^compile term3^" End End"
    | LetIn (name, term1, term2) -> "Begin Push "^name^" "^(compile term1)^" Let "^ (compile term2) ^" End"
    | Fun (fn, arg, term) -> "Fun "^fn^" "^arg^" "^compile term^" End Push "^fn^ " Lookup"
    | App (term1, term2) -> compile term1^" "^compile term2^" Call"
    | Add (term1, term2) -> (compile term1)^" "^(compile term2)^" Add"
    | Sub (term1, term2) -> (compile term1)^" "^(compile term2)^" Sub"
    | Mul (term1, term2) -> (compile term1)^" "^(compile term2)^" Mul"
    | Div (term1, term2) -> (compile term1)^" "^(compile term2)^" Div"
    | Trace (term) -> compile term^" Trace"

let compiler (src : string) : string =
  match parse_prog src with 
  | Some (t, []) -> compile t
  | None -> "Error"
  | _ ->  "Error"



(* test 3: 
src: "let x =
  let x = 0 in
  let y = 1 in
  x
in
let x =
  if x
  then 123
  else 312
in x"
parse_prog src: LetIn ("x", LetIn ("x", Int 0, LetIn ("y", Int 1, Name "x")), LetIn ("x", Ifgz (Name "x", Int 123, Int 312), Name "x"))
out: 
res: 

Begin
  Push loop
  Fun loop n
    Push n
    Lookup
    If
      Begin
        Begin
          Push _
          Push n
          Lookup
          Trace
          Let
          Push loop
          Lookup
          Push n
          Lookup
          Push 2
          Sub
          Call
        End
      End
    Else
      Begin
      Push ()
      End
    End
  End
  *Let*
  Push loop
  Lookup
  Let
  Push loop
  Lookup
  Push 34
  Call
End
*)
