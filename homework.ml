open Env
open Ast
open Code
open Interpreter 

(* PermSet1 = {A} *)
let permset1 = Env.PermSet.empty;;
Env.PermSet.add "A" permset1;;

(* Evaluation of a binding x = 5   *)


(* PermSet2 = {B} *)
let permset1 = Env.PermSet.empty;;
Env.PermSet.add "B" permset1;;

(* Lookup of the variable 'x'. Output should be ERROR. *)


(* PermSet3 = {A, B, C} *)