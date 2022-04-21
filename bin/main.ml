(* permset_A = {A} *)
let permset_A = 
	Lib.Env.PermSet.empty
	|> Lib.Env.PermSet.add "A"
;;
(* permset_B = {B} *)
let permset_B = 
	Lib.Env.PermSet.empty
	|> Lib.Env.PermSet.add "B"
;;
(* permset_C = {C} *)
let permset_C = 
	Lib.Env.PermSet.empty
	|> Lib.Env.PermSet.add "C"
;;
(* permset_AB = {A, B} *)
let _permset_AB = 
	Lib.Env.PermSet.empty
	|> List.fold_right Lib.Env.PermSet.add ["A"; "B"]
;;
(* permset_BC = {B, C} *)
let permset_BC = 
	Lib.Env.PermSet.empty
	|> List.fold_right Lib.Env.PermSet.add ["B"; "C"]
;;
(* permset_AC = {A, C} *)
let _permset_AC = 
	Lib.Env.PermSet.empty
	|> List.fold_right Lib.Env.PermSet.add ["A"; "C"]
;;
(* permset_ABC = {A, B, C} *)
let permset_ABC = 
	Lib.Env.PermSet.empty
	|> List.fold_right Lib.Env.PermSet.add ["A"; "B"; "C"]
;;
(* Function for formatting the battery test, ensuring that the real output matches the expected one *)
let format_test tpair = match tpair with
	| (a, b) -> match b with
		| None -> if a="ABORT" then Printf.printf "OK test\n" else Printf.printf "ERROR test\n"
		| Some _v -> if a="OK" then Printf.printf "OK test\n" else Printf.printf "ERROR test\n"
;;
(* Given an expression 'e' and some set of permissions 'p', it encapsulates the result of an interpreter evaluation.
	When the evaluation aborts ('failwith' case), it will return a None value, otherwise a Some 'v value. *)
let debug e p =
  try Some (e p) with
    Failure(_x) -> None
;;
(* Expression translated in Ocaml syntax: let x = 2 in let y = 3 in let sum_xyz z = z + x + y in sum_xyz 5 
   At time of permission check, Stack should be like this:
    |---Ide---|----Val----|--Perms--|
    |    x    |     2     |    A    |
    |    y    |     3     |    B    |
    | sum_xyz | z->x+y+z  |    C    |
    |---------|-----------|---------|
    Output expected should be 10 *)

let expression = 
		Lib.Ast.Let("x", Eint(2), 
							Let("y", Eint(3), 
								Let("sum_xyz", 
									Fun("z", Op(Sum, Var("z"), Op(Sum, Var("x"), Var("y")))), 
									Call(Var("sum_xyz"), Eint(5)),
								permset_C), 
							permset_B), 
						permset_A)
in
let runtest = Lib.Interpreter.sandbox_eval expression Lib.Env.emptyenv
in List.map (debug runtest) [permset_C; permset_BC; permset_ABC]
	|> List.combine ["ABORT"; "ABORT"; "OK"]
	|> List.iter format_test
;;