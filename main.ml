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
    | Comma
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
    | Arrow
    | Pipe
    (* keywords *)
    | True
    | False
    | And
    | Or
    | If
    | Then
    | Elif
    | Else
    | Let
    | Rec
    | In
    | Match
    | With
  [@@deriving show { with_path = false }]

  let lookup_ident = function
    | "true" -> True
    | "false" -> False
    | "and" -> And
    | "or" -> Or
    | "if" -> If
    | "then" -> Then
    | "elif" -> Elif
    | "else" -> Else
    | "let" -> Let
    | "rec" -> Rec
    | "in" -> In
    | "match" -> Match
    | "with" -> With
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

  let peek_next lexer =
    if lexer.cursor + 1 >= String.length lexer.source then None
    else Some lexer.source.[lexer.cursor + 1]

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

  let skip_whitespace lexer =
    let rec loop () =
      match peek lexer with
      | Some c when Char.is_whitespace c ->
          seek lexer Char.is_whitespace;
          loop ()
      | Some c when c = '/' && peek_next lexer = Some '/' ->
          seek lexer (( <> ) '\n');
          loop ()
      | _ -> ()
    in
    loop ()

  let next_token lexer =
    let open Token in
    seek lexer Char.is_whitespace;
    advance lexer
    |> Option.map @@ function
       (* delims *)
       | '\\' -> Lam
       | '.' -> Dot
       | ',' -> Comma
       | '(' -> LParen
       | ')' -> RParen
       (* operators *)
       | '+' -> Plus
       | '-' -> if matches lexer '>' then Arrow else Minus
       | '*' -> Star
       | '/' -> Slash
       | '=' -> if matches lexer '=' then EqEq else Eq
       | '!' -> if matches lexer '=' then BangEq else Bang
       | '>' -> if matches lexer '=' then GtEq else Gt
       | '<' -> if matches lexer '=' then LtEq else Lt
       | '|' -> Pipe
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
  type lit = Unit | Int of int | Bool of bool
  [@@deriving show { with_path = false }]

  type pattern = Wildcard | Var of string | Lit of lit
  [@@deriving show { with_path = false }]

  type expr =
    | Lit of lit
    | Var of string
    | Bin of (expr * Token.t * expr)
    | Abs of (string * expr)
    | App of (expr * expr)
    | Bind of (bool * string * expr * expr)
    | Cond of (expr * expr * expr)
    | Match of (expr * (pattern * expr) list)
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
    expr     := or ;
    or       := and ('or' and)* ;
    and      := eq ('and' eq)* ;
    eq       := cmp (('==' | '!=') cmp)* ;
    cmp      := term (('>' | '>=' | '<' | '<=') term)* ;
    term     := factor (('+' | '-') factor)* ;
    factor   := operand (('*' | '/') operand)* ;
    operand  :=  bind | cond | match | app ;
    bind     := 'let' 'rec'? ID+ '=' expr 'in' expr ;
    cond     := 'if' expr 'then' expr 'else' expr ;
    match    := 'match' expr 'with' '|'? case ('|' case)* ;
    case     := pattern '->' expr ;
    pattern  := pat_atom ('|' pat_atom)* ;
    pat_atom := '_' | ID | LIT ;
    app      := atom atom* ;
    atom     := ID | LIT | '(' expr ')' | abs ;
    abs      := '\' ID+ '.' expr ;
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
    let expr = parse_operand parser in
    let rec loop expr =
      match parser.current with
      | Token.Star | Token.Slash ->
          advance parser;
          let op = parser.previous in
          let rhs = parse_operand parser in
          loop (Ast.Bin (expr, op, rhs))
      | _ -> expr
    in
    loop expr

  (* operand := bind | cond | match | app ; *)
  and parse_operand parser =
    match parser.current with
    | Token.Let ->
        advance parser;
        parse_bind parser
    | Token.If ->
        advance parser;
        parse_cond parser
    | Token.Match ->
        advance parser;
        parse_match parser
    | _ -> parse_app parser

  (* bind := 'let' 'rec'? ID+ '=' expr 'in' expr *)
  and parse_bind parser =
    let is_recursive = matches parser [ Token.Rec ] in
    let name = consume_ident parser "expected ident after 'let'" in
    (* sugar for binding abstractions:
     let f a b = a + b in <expr>
     let f = \a.\b.a + b in <expr>
     - non-abstraction bindings remain the same: let x = 1 in <expr> *)
    let rec collect params =
      advance parser;
      match parser.previous with
      | Token.Eq -> params
      | Token.Id name -> collect (name :: params)
      | _ -> error "expected '=' or ident after let name"
    in
    let init =
      collect []
      |> List.fold_left
           (fun body param -> Ast.Abs (param, body))
           (parse_expr parser)
    in
    consume parser Token.In "expected 'in' after let initializer";
    let body = parse_expr parser in
    Ast.Bind (is_recursive, name, init, body)

  and parse_collection_with_one parser sep parse_item =
    let rec loop acc =
      if not @@ matches parser [ sep ] then List.rev acc
      else loop (parse_item parser :: acc)
    in
    loop [ parse_item parser ]

  (* cond := 'if' expr 'then' expr 'else' expr ; *)
  and parse_cond parser =
    let cond = parse_expr parser in
    consume parser Token.Then "expected 'then' after  condition";
    let then_branch = parse_expr parser in
    consume parser Token.Else "expected 'else' after 'then' body";
    let else_branch = parse_expr parser in
    Ast.Cond (cond, then_branch, else_branch)

  and parse_branch parser =
    let cond = parse_expr parser in
    consume parser Token.Then "expected 'then' after  condition";
    (cond, parse_expr parser)

  (* match := 'match' expr 'with' '|'? case ('|' case)* ; *)
  and parse_match parser =
    let scrutinee = parse_expr parser in
    consume parser Token.With "expected 'with' after match scrutinee";
    if parser.current = Token.Pipe then advance parser;
    let cases = parse_collection_with_one parser Token.Pipe parse_case in
    (* let first = parse_case parser in *)
    (* let rec cases acc = *)
    (*   if not @@ matches parser [ Token.Pipe ] then List.rev acc *)
    (*   else cases (parse_case parser :: acc) *)
    (* in *)
    Ast.Match (scrutinee, cases)

  (* case := pattern '->' expr ; *)
  and parse_case parser =
    let pat = parse_pattern parser in
    consume parser Token.Arrow "expected '->' after pattern";
    let body = parse_expr parser in
    (pat, body)

  (* pattern  := pat_atom ('|' pat_atom)* ; *)
  (* pat_atom := '_' | ID | LIT ; *)
  and parse_pattern parser =
    advance parser;
    match parser.previous with
    | Token.Id name when name = "_" -> Ast.Wildcard
    | Token.Id name -> Ast.Var name
    | Token.Num n -> Ast.Lit (Int (int_of_string n))
    | Token.True -> Ast.Lit (Bool true)
    | Token.False -> Ast.Lit (Bool false)
    | _ -> error "expected pattern"

  (** app := atom atom* ; *)
  and parse_app parser =
    let expr = parse_atom parser in
    let rec loop expr =
      match parser.current with
      | Token.Id _ | Token.Num _ | Token.True | Token.False | Token.LParen
      | Token.Lam ->
          let rhs = parse_atom parser in
          loop (Ast.App (expr, rhs))
      | _ -> expr
    in
    loop expr

  (** atom := ID | LIT | '(' expr ')' | abs ; *)
  and parse_atom parser =
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
    | _ -> error "expected expression"

  (** abs := '\' ID+ '.' expr ; *)
  and parse_abs parser =
    let param = consume_ident parser "expected ident after '\\'" in
    (* nested abs syntax sugar -> \x y.x == \x.\y.x *)
    let rec loop param =
      advance parser;
      match parser.previous with
      | Token.Dot -> Ast.Abs (param, parse_expr parser)
      | Token.Id name -> Ast.Abs (param, loop name)
      | _ -> error "expected '.' or ident"
    in
    loop param
end

module Lower = struct
  let rec lower expr = lower_match expr

  and lower_match = function
    | Ast.Match (scrutinee, cases) ->
        let scrutinee = lower_match scrutinee in
        lower_cases scrutinee cases
    | Lit _ as lit -> lit
    | Var _ as v -> v
    | Bin (l, op, r) -> Bin (lower_match l, op, lower_match r)
    | Abs (x, body) -> Abs (x, lower_match body)
    | App (f, arg) -> App (lower_match f, lower_match arg)
    | Bind (is_rec, name, init, body) ->
        Bind (is_rec, name, lower_match init, lower_match body)
    | Cond (c, t, e) -> Cond (lower_match c, lower_match t, lower_match e)

  (*
  let x = 0 in match x with 0 -> 100 | 1 -> 200 |  n -> n + 1

  let x = 0 in
    if x == 0 then 100
    else
      if n == 1 then 200
      else
        let n = x in
           n + 1
 *)
  and lower_cases scrutinee = function
    | [] -> failwith "non-exhausitve match"
    | (pat, body) :: rest -> (
        let body = lower_match body in
        match pat with
        | Wildcard -> body
        | Var name -> Bind (false, name, scrutinee, body)
        | Lit lit_val ->
            let cond = Ast.Bin (scrutinee, Token.EqEq, Lit lit_val) in
            let else_branch = lower_cases scrutinee rest in
            Cond (cond, body, else_branch))
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
      | Fun of (string * Ast.expr * t ref Env.t)
      | Native of (t -> t)
    (* | Patch (* placeholder for recursive bindings *) *)

    let show = function
      | Unit -> "()"
      | Int n -> string_of_int n
      | Bool b -> string_of_bool b
      | Fun _ | Native _ -> "<fn>"

    let show_with_type v =
      match v with
      | Unit -> format "unit = %s" (show v)
      | Int _ -> format "int = %s" (show v)
      | Bool _ -> format "bool = %s" (show v)
      | Fun (input, _, _) -> format "%s -> ? = %s" input (show v)
      | Native _ -> format "? -> ? = %s" (show v)

    let is_fn = function Fun _ -> true | _ -> false
  end

  open Ast
  open Value

  let lookup env name =
    match Env.find_opt name env with
    | Some r -> !r
    | None -> error (format "unbound var %s" name)

  let get_number_operands lhs rhs =
    match (lhs, rhs) with
    | Int l, Int r -> (l, r)
    | _ -> error "operands must be numbers"

  let bin_cmp lhs op rhs =
    let l, r = get_number_operands lhs rhs in
    Bool (op l r)

  let bin_ari lhs op rhs =
    let l, r = get_number_operands lhs rhs in
    Int (op l r)

  let rec eval' env = function
    | Lit Unit -> Unit
    | Lit (Int n) -> Int n
    | Lit (Bool b) -> Bool b
    | Var name -> lookup env name
    | Abs (param, body) -> Fun (param, body, env)
    | App (_ as app) -> eval_app env app
    | Bin (_ as bin) -> eval_bin env bin
    | Bind (_ as bind) -> eval_bind env bind
    | Cond (_ as cond) -> eval_cond env cond
    | Match _ -> assert false

  and eval_app env (lhs, rhs) =
    let lhs = eval' env lhs in
    let rhs = eval' env rhs in

    (*

    (\x. \y. x + y) 2 3

    eval' {} app(app(abs(x, abs(y, bin(x, +, y))), 2), 3) ->
      eval_app ->
        lhs = eval' {} app(abs(x, abs(y, bin(x, +, y))), 2) ->
          lhs = eval {} abs(x, abs(y, bin(x, +, y)))
          rhs = 2
          eval' { "x": 2 } abs(y, bin(x, +, y)) (now first lhs holds this fun with its env mapped) -> 
        rhs = 3
        eval { "x": 2, "y": 3 } bin(x, +, y) ->
          2 + 3 (the env of the caller has never been modified)
    *)

    (* apply *)
    match lhs with
    | Native f -> f rhs
    | Fun (param, body, env') -> eval' (Env.add param (ref rhs) env') body
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

  and eval_bind env (is_recursive, name, init, body) =
    if not is_recursive then
      let init = eval' env init in
      eval' (Env.add name (ref init) env) body
    else
      let cell = ref Unit in
      let env = Env.add name cell env in
      (* todo: check if init is fn and is recursive *)
      cell := eval' env init;
      eval' env body

  and eval_cond env (cond, then_branch, else_branch) =
    match eval' env cond with
    | Bool true -> eval' env then_branch
    | Bool false -> eval' env else_branch
    | _ -> error "only booleans are allowed in conditions"

  let add_native name f = Env.add name (ref (Native f))

  let init_env () =
    Env.empty
    |> add_native "echo" @@ fun v ->
       Value.show v |> print_endline;
       Unit

  let eval = eval' (init_env ())
end

type interpret_error = Parse_error of string | Runtime_error of string

let interpret source =
  try
    Ok
      (Parser.parse source
      |> Option.map @@ fun expr ->
         Ast.show_expr expr |> println "before lower: %s";
         let expr = Lower.lower expr in
         Ast.show_expr expr |> println "after lower: %s";
         Runtime.eval expr)
  with
  | Parser.Exn reason -> Error (Parse_error reason)
  | Runtime.Exn reason -> Error (Runtime_error reason)

let rec run_repl () =
  print_string "\x1b[34mλ>\x1b[0m ";
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
