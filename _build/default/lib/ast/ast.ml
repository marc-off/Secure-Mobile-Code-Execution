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
  | Let of ide * expr * expr * Code.domain
  (* Extended the semantics of function evaluation to include the notion of domain, inside which the function is defined *)
  | Fun of ide * expr
  (* Extended the semantics of function call, to include the domain of the called function *)
  | Call of expr * expr
  | Read of ide (* Read operation *)
  | Write of ide (* Write operation *)
  | Send of ide (* Send Operation *)
	(* | Delete Delete Operation *)
;;
(* Evaluation of AST brings to these specific values *)
type value = 
  | Int of int 
  | Bool of bool
  | Closure of  (ide*expr*value Env.env)
;;
