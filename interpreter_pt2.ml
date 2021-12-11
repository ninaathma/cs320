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

let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = disj

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

(* part1 AST *)

type name = string

type const = (* const ::= nat | name | unit *)
  | I of int
  | N of string
  | U

type cmd =
  | Push of const
  | Add | Sub | Mul | Div 
  | Trace 
  | Ifgz  of cmds * cmds 
  | Let
  | Lookup
  | BeginEnd of cmds
  | FunEnd of const * const * cmds
  | Call
and cmds = cmd list

(* part1 parser *)

let reserved = [
  "Push";
  "Add";
  "Sub";
  "Mul";
  "Div";
  "Trace";
  "Let";
  "If";
  "Else";
  "Fun";
  "End";
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

let nat_parser () =
  let* n = natural in
  pure (I n) << ws

let name_parser () =
  let* n = name in
  pure (N n)

let unit_parser () =
  let* _ = keyword "()" in
  pure (U)

let const_parser () =
  nat_parser () <|>
  name_parser () <|>
  unit_parser ()

let rec push_parser () =
  let* _ = keyword "Push" in
  let* cst = const_parser () in
  pure (Push cst)

and add_parser () =
  let* _ = keyword "Add" in
  pure (Add)

and sub_parser () =
  let* _ = keyword "Sub" in
  pure (Sub)

and mul_parser () =
  let* _ = keyword "Mul" in
  pure (Mul)

and div_parser () =
  let* _ = keyword "Div" in
  pure (Div)

and trace_parser () =
  let* _ = keyword "Trace" in
  pure (Trace)

and ifgz_parser () =
  let* _ = keyword "If" in
  let* cmds1 = cmds_parser () in
  let* _ = keyword "Else" in
  let* cmds2 = cmds_parser () in
  let* _ = keyword "End" in
  pure (Ifgz (cmds1, cmds2))
and let_parser () =
  let* _ = keyword "Let" in
  pure(Let)
and lookup_parser () =
  let* _ = keyword "Lookup" in
  pure(Lookup)

and beginend_parser() =
  let* _ = keyword "Begin" in
  let* cmdsInner = cmds_parser () in
  let* _ = keyword "End" in
  pure (BeginEnd(cmdsInner))

and funend_parser()=
  let* _ = keyword "Fun" in
  let* fname = name_parser() in
  let* arg = name_parser() in
  let* cmdsInner = cmds_parser() in
  let* _ = keyword "End" in
  pure (FunEnd(fname, arg, cmdsInner))

and call_parser()=
  let* _ = keyword "Call" in
  pure(Call)

and cmd_parser () = 
  push_parser () <|>
  add_parser () <|>
  sub_parser () <|>
  mul_parser () <|>
  div_parser () <|>
  trace_parser () <|>
  ifgz_parser () <|>
  let_parser() <|>
  lookup_parser() <|>
  beginend_parser() <|>
  funend_parser() <|>
  call_parser()


and cmds_parser () =
  many (cmd_parser ())

let parse_cmds = 
  parse (
    ws >> 
    cmds_parser ()
  )


(* interpreter *)

type value =
  | IVal of int
  | NVal of string
  | UVal
  | CVal of name * name * cmds * env  
and env = (name * value) list

type stack = value list
type result =
  | Ok of stack
  | Error


let rec string_of_value v =
  match v with
  | IVal n -> string_of_int n
  | NVal s -> s
  | UVal -> "()"
  | CVal (n1, n2, cmds, env) -> "<fun>"

let string_of_result res =
  match res with
  | Ok (v :: _) -> string_of_value v
  | _ -> "Error"

let string_of_log log = 
  let rec loop log =
    match log with
    | [] -> ""
    | s :: [] -> s
    | s :: log -> s ^ "; " ^ loop log
  in
  "[" ^ loop log ^ "]"

let rec lookup str env =
  match env with
    | (s, i) :: tl -> if s=str then Some i else lookup str tl
    | [] -> None

 
let rec interp cmds stack log env =
match cmds with
| (Push (I n)) :: cmds ->
  interp cmds ((IVal n) :: stack) log env
| (Push (N s)) :: cmds ->
  interp cmds ((NVal s) :: stack) log env
| (Push U) :: cmds ->
  interp cmds (UVal :: stack) log env
| Add :: cmds -> (
  match stack with
  | IVal n :: IVal m :: st -> interp cmds (IVal (m + n) :: st) log env
  | _ -> (Error, log))
| Sub :: cmds -> (
  match stack with
  | IVal n :: IVal m :: st -> interp cmds (IVal (m - n) :: st) log env
  | _ -> (Error, log))
| Mul :: cmds -> (
  match stack with
  | IVal n :: IVal m :: st -> interp cmds (IVal (m * n) :: st) log env
  | _ -> (Error, log))
| Div :: cmds -> (
  match stack with
  | IVal 0 :: IVal _ :: _ -> (Error, log)
  | IVal n :: IVal m :: st -> interp cmds (IVal (m / n) :: st) log env
  | _ -> (Error, log))
| Trace :: cmds -> (
  match stack with
  | v :: st -> interp cmds (UVal :: st) (string_of_value v :: log) env
  | _ -> (Error, log))
| Ifgz (cmds1, cmds2) :: cmds -> (
  match stack with
  | IVal n :: st ->
    if n > 0
    then interp (cmds1 @ cmds) st log env
    else interp (cmds2 @ cmds) st log env
  | _ -> (Error, log))
| Let :: cmds -> (
  match stack with
  | v :: NVal n :: st -> interp cmds st log ((n, v)::env)
  | _ -> (Error,log))
| Lookup :: cmds -> ( 
  match stack with
  | NVal n :: st -> 
    (match lookup n env with
      | Some v -> interp cmds (v::st) log env
      | None -> (Error, log))
  | _ -> (Error, log))
| BeginEnd (cmdsInner)::cmds -> let envInner=env in let stackInner=[] in
  (match interp cmdsInner stackInner log envInner with
  | (Ok (v::stInner), log) -> interp cmds (v :: stack) log env (* terminates and reports error if inner stack is empty after exiting BeginEnd block *)
  | _ -> (Error, log))
| FunEnd (fname, arg, cmdsInner)::cmds ->
  (match fname, arg with 
    | N f, N x -> interp cmds stack log ((f, CVal (f, x, cmdsInner, env))::env)
    | _ -> (Error, log))
| Call::cmds -> 
  (match stack with
  | v :: CVal (f, arg, cmdsInner, e) :: st -> 
  (match interp cmdsInner [] log ( (f, CVal (f, arg, cmdsInner, e))::(arg, v)::e ) with 
    | (Ok (a::stInner), log) -> interp cmds (a::st) log env
    | _ -> (Error, log)) 
  | _ -> (Error,log))
| [] -> (Ok stack, log)


(* end of parser combinators *)

(* Interprets a program written in the Part1 Stack Language.
 * Required by the autograder, do not change its type. *)
let interpreter src =
  match parse_cmds src with
  | Some (cmds, []) -> (
    match (interp cmds [] [] []) with
    | Ok (v :: _), logs -> (string_of_value v, logs)
    | _ -> ("Error", []) )
  | _ -> ("Error", []) 

(* Testing function. *)
let main fname = 
  let src = readlines fname in
  let (res, log) = interpreter src in
  print_endline (res ^ " " ^ string_of_log log)