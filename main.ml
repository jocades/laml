module Option = struct
  include Option

  let is_some_and f = function Some x -> f x | None -> false
  let and_then f = function Some x -> f x | None -> None
  let get_or_else f = function Some x -> x | None -> f ()
end

module Result = struct
  include Result

  let and_then f = function Ok x -> f x | Error _ as e -> e
  let get_or_else f = function Ok x -> x | Error u -> f u
end

module Char = struct
  include Char

  let is_whitespace = function '\t' | '\n' | '\r' | ' ' -> true | _ -> false
  let is_alpha = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_alnum c = is_alpha c || is_digit c
end

module Bool = struct
  include Bool

  let then_some f b = if b then Some (f ()) else None
end

let format fmt = Format.sprintf fmt
let println fmt = Format.printf (fmt ^^ "@.")
let eprintln fmt = Format.eprintf (fmt ^^ "@.")

module Token = struct
  type t =
    | Eof
    (* items *)
    | Id of string
    | Num of string
    (* delims *)
    | Lam
    | Dot
    | LParen
    | RParen
    (* operators *)
    | Plus
    | Minus
    | Star
    | Slash
    | Eq
    | EqEq
    | Bang
    | BangEq
    | Gt
    | GtEq
    | Lt
    | LtEq
    (* keywords *)
    | True
    | False
    | And
    | Or
    | If
    | Then
    | Else
    | Let
    | In
  [@@deriving show { with_path = false }]

  let lookup_ident = function
    | "true" -> True
    | "false" -> False
    | "and" -> And
    | "or" -> Or
    | "if" -> If
    | "then" -> Then
    | "else" -> Else
    | "let" -> Let
    | "in" -> In
    | x -> Id x
end

module Lexer : sig
  type t

  val init : string -> t
  val next_token : t -> Token.t option
end = struct
  type t = { source : string; mutable cursor : int }

  let init source = { source; cursor = 0 }

  let peek lexer =
    if lexer.cursor >= String.length lexer.source then None
    else Some lexer.source.[lexer.cursor]

  let advance lexer =
    peek lexer
    |> Option.map @@ fun c ->
       lexer.cursor <- lexer.cursor + 1;
       c

  let seek lexer pred =
    let rec loop () =
      if peek lexer |> Option.is_some_and pred then
        let _ = advance lexer in
        loop ()
    in
    loop ()

  let matches lexer m =
    if peek lexer |> Option.is_some_and (( = ) m) then
      let _ = advance lexer in
      true
    else false

  let next_token lexer =
    let open Token in
    seek lexer Char.is_whitespace;
    advance lexer
    |> Option.map @@ function
       (* delims *)
       | '\\' -> Lam
       | '.' -> Dot
       | '(' -> LParen
       | ')' -> RParen
       (* operators *)
       | '+' -> Plus
       | '-' -> Minus
       | '*' -> Star
       | '/' -> Slash
       | '=' -> if matches lexer '=' then EqEq else Eq
       | '!' -> if matches lexer '=' then BangEq else Bang
       | '>' -> if matches lexer '=' then GtEq else Gt
       | '<' -> if matches lexer '=' then LtEq else Lt
       (* items *)
       | c when c = '_' || Char.is_alpha c ->
           let start = lexer.cursor - 1 in
           seek lexer (fun c -> c = '_' || Char.is_alnum c);
           String.sub lexer.source start (lexer.cursor - start)
           |> Token.lookup_ident
       | c when Char.is_digit c ->
           let start = lexer.cursor - 1 in
           seek lexer Char.is_digit;
           Num (String.sub lexer.source start (lexer.cursor - start))
       | c -> failwith (format "unexpected character '%c'" c)

  let debug_tokens source =
    let rec loop lexer =
      next_token lexer
      |> Option.iter @@ fun token ->
         Token.show token |> print_endline;
         loop lexer
    in
    loop (init source)
end

module Ast = struct
  type expr =
    | Lit of lit
    | Var of string
    | Bin of (expr * Token.t * expr)
    | Abs of (string * expr)
    | App of (expr * expr)
    | Bind of (string * expr * expr)
    | Cond of (expr * expr * expr)
  [@@deriving show { with_path = false }]

  and lit = Unit | Int of int | Bool of bool
  [@@deriving show { with_path = false }]

  let show_expr_list = [%show: expr list]
end

module Parser : sig
  type t

  exception Exn of string

  val init : string -> t
  val parse : string -> Ast.expr option
end = struct
  (*
    expr   := or ;
    or     := and ('or' and)* ;
    and    := eq ('and' eq)* ;
    eq     := cmp (('==' | '!=') cmp)* ;
    cmp    := term (('>' | '>=' | '<' | '<=') term)* ;
    term   := factor (('+' | '-') factor)* ;
    factor := app (('*' | '/') app)* ;
    app    := atom atom* ;
    atom   := ID | LIT | '(' expr ')' | abs | cond | bind ;
    abs    := '\' ID '.' expr ;
    cond   := 'if' expr 'then' expr else 'expr' ;
    bind   := 'let' ID 'in' expr ;
  *)

  type t = {
    lexer : Lexer.t;
    mutable current : Token.t;
    mutable previous : Token.t;
  }

  exception Exn of string

  let init source =
    { lexer = Lexer.init source; current = Token.Eof; previous = Token.Eof }

  let error reason = raise (Exn reason)

  let advance parser =
    parser.previous <- parser.current;
    match Lexer.next_token parser.lexer with
    | Some token -> parser.current <- token
    | None -> parser.current <- Token.Eof

  let matches parser tokens =
    if List.find_opt (( = ) parser.current) tokens |> Option.is_some then (
      advance parser;
      true)
    else false

  let consume parser token reason =
    if parser.current = token then advance parser else error reason

  let consume_ident parser reason =
    match parser.current with
    | Token.Id name ->
        advance parser;
        name
    | _ -> error reason

  let rec parse source =
    let parser = init source in
    advance parser;
    parser.current <> Token.Eof |> Bool.then_some (fun _ -> parse_expr parser)

  (* expr := or ; *)
  and parse_expr parser = parse_or parser

  (* or := and ('or' and)* ; *)
  and parse_or parser =
    let expr = parse_and parser in
    let rec loop expr =
      match parser.current with
      | Token.Or ->
          advance parser;
          let op = parser.previous in
          let rhs = parse_and parser in
          loop (Ast.Bin (expr, op, rhs))
      | _ -> expr
    in
    loop expr

  (* and := eq ('and' eq)* ; *)
  and parse_and parser =
    let expr = parse_eq parser in
    let rec loop expr =
      match parser.current with
      | Token.And ->
          advance parser;
          let op = parser.previous in
          let rhs = parse_eq parser in
          loop (Ast.Bin (expr, op, rhs))
      | _ -> expr
    in
    loop expr

  (** eq := cmp (('==' | '!=') cmp)* ; *)
  and parse_eq parser =
    let expr = parse_cmp parser in
    let rec loop expr =
      match parser.current with
      | Token.EqEq | Token.BangEq ->
          advance parser;
          let op = parser.previous in
          let rhs = parse_cmp parser in
          loop (Ast.Bin (expr, op, rhs))
      | _ -> expr
    in
    loop expr

  (** cmp := term (('>' | '>=' | '<' | '<=') term)* ; *)
  and parse_cmp parser =
    let expr = parse_term parser in
    let rec loop expr =
      match parser.current with
      | Token.Gt | Token.GtEq | Token.Lt | Token.LtEq ->
          advance parser;
          let op = parser.previous in
          let rhs = parse_term parser in
          loop (Ast.Bin (expr, op, rhs))
      | _ -> expr
    in
    loop expr

  (** term := factor (('+' | '-') factor)* ; *)
  and parse_term parser =
    let expr = parse_factor parser in
    let rec loop expr =
      match parser.current with
      | Token.Plus | Token.Minus ->
          advance parser;
          let op = parser.previous in
          let rhs = parse_factor parser in
          loop (Ast.Bin (expr, op, rhs))
      | _ -> expr
    in
    loop expr

  (** factor := app (('*' | '/') app)* ; *)
  and parse_factor parser =
    let expr = parse_app parser in
    let rec loop expr =
      match parser.current with
      | Token.Star | Token.Slash ->
          advance parser;
          let op = parser.previous in
          let rhs = parse_app parser in
          loop (Ast.Bin (expr, op, rhs))
      | _ -> expr
    in
    loop expr

  (** app := atom atom* ; *)
  and parse_app parser =
    let expr = parse_primary parser in
    let rec loop expr =
      match parser.current with
      | Token.Id _ | Token.Num _ | Token.LParen | Token.Lam ->
          let rhs = parse_primary parser in
          loop (Ast.App (expr, rhs))
      | _ -> expr
    in
    loop expr

  (** atom := ID | LIT | '(' expr ')' | abs ; *)
  and parse_primary parser =
    advance parser;
    match parser.previous with
    | Token.Id name -> Ast.Var name
    | Token.Num x -> Ast.Lit (Int (int_of_string x))
    | Token.True -> Ast.Lit (Bool true)
    | Token.False -> Ast.Lit (Bool false)
    | Token.LParen ->
        if matches parser [ Token.RParen ] then Ast.Lit Unit
        else
          let expr = parse_expr parser in
          consume parser Token.RParen "expected ')' after grouping";
          expr
    | Token.Lam -> parse_abs parser
    | Token.Let -> parse_bind parser
    | Token.If -> parse_cond parser
    | _ -> error "expected expression"

  (** abs := '\' ID '.' expr ; *)
  and parse_abs parser =
    let input = consume_ident parser "expected ident after '\\'" in
    (* nested abs syntax sugar -> \x y.x == \x.\y.x
       abs := '\' ID+ '.' expr ; *)
    let rec loop input =
      advance parser;
      match parser.previous with
      | Token.Dot -> Ast.Abs (input, parse_expr parser)
      | Token.Id name -> Ast.Abs (input, loop name)
      | _ -> error "expected '.' or ident"
    in
    loop input

  (** bind := 'let' ID '=' expr 'in' expr *)
  and parse_bind parser =
    let name = consume_ident parser "expected ident after 'let'" in
    consume parser Token.Eq "expected '=' after let name";
    let init = parse_expr parser in
    consume parser Token.In "expected 'in' after let initializer";
    let expr = parse_expr parser in
    Ast.Bind (name, init, expr)

  (* cond := 'if' expr 'then' expr else 'expr' ; *)
  and parse_cond parser =
    let cond = parse_expr parser in
    consume parser Token.Then "expected 'then' after  condition";
    let then_branch = parse_expr parser in
    consume parser Token.Else "expected 'else' after 'then' expression";
    let else_branch = parse_expr parser in
    Ast.Cond (cond, then_branch, else_branch)
end

module Runtime = struct
  exception Exn of string

  let error reason = raise (Exn reason)

  module Env = Map.Make (String)

  module Value = struct
    type t =
      | Unit
      | Int of int
      | Bool of bool
      | Fun of (string * Ast.expr * t Env.t)
      | Native of (string * (t -> t))

    let rec show = function
      | Unit -> "()"
      | Int n -> string_of_int n
      | Bool b -> string_of_bool b
      | Fun _ -> "<fn>"
      | Native (name, _) -> format "<native fn '%s'>" name

    let rec show_with_type v =
      match v with
      | Unit -> format "unit = %s" (show v)
      | Int _ -> format "int = %s" (show v)
      | Bool _ -> format "bool = %s" (show v)
      | Fun (input, _, _) -> format "%s -> ? = %s" input (show v)
      | Native (_, _) -> format "? -> ? = %s" (show v)
  end

  open Ast
  open Value

  let bin_cmp lhs op rhs =
    match (lhs, rhs) with
    | Int l, Int r -> Bool (op l r)
    | _ -> error "operands must be numbers"

  let bin_ari lhs op rhs =
    match (lhs, rhs) with
    | Int l, Int r -> Int (op l r)
    | _ -> error "operands must be numbers"

  let rec eval' env = function
    | Lit Unit -> Unit
    | Lit (Int n) -> Int n
    | Lit (Bool b) -> Bool b
    | Var name ->
        Env.find_opt name env
        |> Option.get_or_else (fun _ -> error (format "unbound var %s" name))
    | Abs (input, expr) -> Fun (input, expr, env)
    | App (_ as app) -> eval_app env app
    | Bin (_ as bin) -> eval_bin env bin
    | Bind (_ as bind) -> eval_bind env bind
    | Cond (_ as cond) -> eval_cond env cond

  and eval_app env (lhs, rhs) =
    let lhs = eval' env lhs in
    let rhs = eval' env rhs in
    match lhs with
    | Native (_, f) -> f rhs
    | Fun (input, expr, env') -> eval' (Env.add input rhs env') expr
    | _ -> error "only functions are callable"

  and eval_bin env (lhs, op, rhs) =
    let l, r = (eval' env lhs, eval' env rhs) in
    match op with
    | Token.EqEq -> Bool (l = r)
    | Token.BangEq -> Bool (l <> r)
    | Token.Gt -> bin_cmp l ( > ) r
    | Token.GtEq -> bin_cmp l ( >= ) r
    | Token.Lt -> bin_cmp l ( < ) r
    | Token.LtEq -> bin_cmp l ( <= ) r
    | Token.Plus -> bin_ari l ( + ) r
    | Token.Minus -> bin_ari l ( - ) r
    | Token.Star -> bin_ari l ( * ) r
    | Token.Slash -> bin_ari l ( / ) r
    | _ -> assert false

  and eval_bind env (name, init, expr) =
    let init = eval' env init in
    eval' (Env.add name init env) expr

  and eval_cond env (cond, then_branch, else_branch) =
    match eval' env cond with
    | Bool b -> if b then eval' env then_branch else eval' env else_branch
    | _ -> error "only booleans are allowed in conditions"

  let add_native name f = Env.add name (Native (name, f))

  let init_env () =
    Env.empty
    |> add_native "echo" @@ fun v ->
       Value.show v |> print_endline;
       Unit

  let eval = eval' (init_env ())
end

type interpret_error = Parse_error of string | Runtime_error of string

let interpret source =
  try Ok (Parser.parse source |> Option.map Runtime.eval) with
  | Parser.Exn reason -> Error (Parse_error reason)
  | Runtime.Exn reason -> Error (Runtime_error reason)

let rec run_repl () =
  try
    (match read_line () |> interpret with
    | Ok (Some value) -> Runtime.Value.show_with_type value |> println "- : %s"
    | Error (Parse_error reason) -> eprintln "parse error: %s" reason
    | Error (Runtime_error reason) -> eprintln "runtime error: %s" reason
    | _ -> ());
    run_repl ()
  with End_of_file -> ()

let read_file_to_string path =
  try Ok (In_channel.with_open_text path In_channel.input_all)
  with Sys_error reason -> Error reason

let run_file path =
  let source =
    read_file_to_string path
    |> Result.get_or_else @@ fun reason ->
       eprintln "failed to read file: %s" reason;
       exit 1
  in
  match interpret source with
  | Ok (Some value) -> Runtime.Value.show value |> print_endline
  | Error (Parse_error reason) -> eprintln "parse error: %s" reason
  | Error (Runtime_error reason) -> eprintln "runtime error: %s" reason
  | _ -> ()

let () =
  match Array.length Sys.argv with
  | 1 -> run_repl ()
  | 2 -> run_file Sys.argv.(1)
  | _ -> eprintln "usage: %s [path]" Sys.argv.(0)
