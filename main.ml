module Option = struct
  include Option

  let is_some_and f = function Some x -> f x | None -> false
  let and_then f = function Some x -> f x | None -> None
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
    | Id of string
    | Num of string
    | Lam
    | Dot
    | LParen
    | RParen
    | Plus
    | Minus
    | Star
    | Slash
    | Eof
  [@@deriving show { with_path = false }]
end

module Lexer = struct
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

  let next_token lexer =
    let open Token in
    seek lexer Char.is_whitespace;
    advance lexer
    |> Option.map @@ function
       | '\\' -> Lam
       | '.' -> Dot
       | '(' -> LParen
       | ')' -> RParen
       | '+' -> Plus
       | '-' -> Minus
       | '*' -> Star
       | '/' -> Slash
       | c when c = '_' || Char.is_alpha c ->
           let start = lexer.cursor - 1 in
           seek lexer (fun c -> c = '_' || Char.is_alnum c);
           Id (String.sub lexer.source start (lexer.cursor - start))
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
    | Lit of int
    | Var of string
    | Bin of expr * Token.t * expr
    | Abs of string * expr
    | App of expr * expr
  [@@deriving show { with_path = false }]

  let show_expr_list = [%show: expr list]
end

module Parser = struct
  (*
    expr   := term ;
    term   := factor (('+' | '-') factor)* ;
    factor := app (('*' | '/') app)* ;
    app    := atom atom* ;
    atom   := ID | LIT | '(' expr ')' | abs ;
    abs    := '\' ID '.' expr ;
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

  let consume parser x reason =
    if parser.current = x then advance parser else error reason

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

  and parse_expr parser = parse_term parser

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
    | Token.Num x -> Ast.Lit (int_of_string x)
    | Token.LParen ->
        let expr = parse_expr parser in
        consume parser Token.RParen "expected ')' after grouping";
        expr
    | Token.Lam -> parse_abs parser
    | _ -> error "expected expression"

  (** abs := '\' ID '.' expr ; *)
  and parse_abs parser =
    let param = consume_ident parser "expected ident after '\\'" in
    (* nested abs syntax sugar -> \x y.x == \x.\y.x
       abs := '\' ID+ '.' expr ; *)
    let rec loop param =
      advance parser;
      match parser.previous with
      | Token.Dot -> Ast.Abs (param, parse_expr parser)
      | Token.Id name -> Ast.Abs (param, loop name)
      | _ -> error "expected '.' or ident"
    in
    loop param
end

module Runtime = struct
  open Ast

  exception Exn of string

  let error reason = raise (Exn reason)
  let as_int = function Lit x -> x | _ -> error "expected int"

  let rec subst (src : string) (dst : expr) (expr : expr) =
    let subst' = subst src dst in
    match expr with
    | Lit _ -> expr
    | Var name -> if src = name then dst else expr
    | Bin (lhs, op, rhs) -> Bin (subst' lhs, op, subst' rhs)
    | Abs (param, body) -> if src = param then expr else Abs (param, subst' body)
    | App (lhs, rhs) -> App (subst' lhs, subst' rhs)

  let rec reduce = function
    | (Lit _ | Var _ | Abs _) as expr -> expr
    | Bin (lhs, op, rhs) ->
        let lhs = reduce lhs |> as_int in
        let rhs = reduce rhs |> as_int in
        Lit
          (match op with
          | Token.Plus -> lhs + rhs
          | Token.Minus -> lhs - rhs
          | Token.Star -> lhs * rhs
          | Token.Slash -> lhs / rhs
          | _ -> error ("unkown op " ^ Token.show op))
    | App (lhs, rhs) -> (
        match reduce lhs with
        | Abs (param, body) -> subst param rhs body |> reduce
        | _ -> error "expected abs")
end

type eval_error = ParseError of string | RuntimeError of string

let eval source : (Ast.expr option, eval_error) result =
  try
    match Parser.parse source with
    | Some expr -> Ok (Some (Runtime.reduce expr))
    | None -> Ok None
  with
  | Parser.Exn reason -> Error (ParseError reason)
  | Runtime.Exn reason -> Error (RuntimeError reason)

let rec repl () =
  try
    (match read_line () |> eval with
    | Ok (Some expr) -> Ast.show_expr expr |> print_endline
    | Error (ParseError reason) -> eprintln "parse error: %s" reason
    | Error (RuntimeError reason) -> eprintln "runtime error: %s" reason
    | _ -> ());
    repl ()
  with End_of_file -> ()

let () = repl ()
