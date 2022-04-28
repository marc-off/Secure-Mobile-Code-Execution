(* Identifier for functions and variables *)
type ide = string
;;
(* Primary operations between values *)
type op = Sum | Minus | Times | Equal | Less | Greater
;;
(* Abstract Syntax of a program *)
type expr =
  | CstTrue
	| CstFalse
	| Eint of int
  | Var of ide
  | Op of op * expr * expr
  | If of expr * expr * expr
  (* Extended the semantics of a Let, to define the permissions for accessing a variable *)
  | Let of ide * expr * expr * Code.domain
  | Fun of ide * expr
  | Call of expr * expr
  | Read of ide (* Read operation *)
  | Write of ide (* Write operation *)
  | Open of ide (* Open operation *)
  | Send of ide (* Send Operation *)
;;
(* Evaluation of AST brings to these specific values *)
type value =
  | Int of int
  | Bool of bool
  | Closure of  (ide*expr*value Env.env)
;;
