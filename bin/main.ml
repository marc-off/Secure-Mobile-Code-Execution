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
let permset_AB = 
	Lib.Env.PermSet.empty
	|> List.fold_right Lib.Env.PermSet.add ["A"; "B"]
;;
(* permset_BC = {B, C} *)
let permset_BC = 
	Lib.Env.PermSet.empty
	|> List.fold_right Lib.Env.PermSet.add ["B"; "C"]
;;
(* permset_AC = {A, C} *)
let permset_AC = 
	Lib.Env.PermSet.empty
	|> List.fold_right Lib.Env.PermSet.add ["A"; "C"]
;;
(* permset_ABC = {A, B, C} *)
let permset_ABC = 
	Lib.Env.PermSet.empty
	|> List.fold_right Lib.Env.PermSet.add ["A"; "B"; "C"]
;;

let (domains: Code.domain list) = [
	{domain = "https://www.ryanair.com/";
		read_perms = permset_A; write_perms =  permset_C; send_perms = permset_AC};
	{domain = "https://www.google.it/";
		read_perms = permset_B; write_perms =  permset_A; send_perms = permset_AB};
	{domain = "https://www.apple.com/it/";
		read_perms = permset_ABC; write_perms = permset_C; send_perms = permset_A};
	{domain = "https://www.amazon.it/";
		read_perms = permset_C; write_perms = permset_AC; send_perms = permset_BC};
];;
(* Function for formatting the battery test, ensuring that the real output matches the expected one *)
let format_test tpair = match tpair with
	| (a, b) -> match b with
		| None -> if a="ABORT" then Printf.printf "OK test\n" else Printf.printf "ERROR test\n"
		| Some _v -> if a="OK" then Printf.printf "OK test\n" else Printf.printf "ERROR test\n"
;;
(* Given an expression 'e' and some set of permissions 'p', it encapsulates the result of an interpreter evaluation.
	When the evaluation aborts ('failwith' case), it will return a None value, otherwise a Some 'v value. *)
let encapsulate_eval e p =
  try Some (e p) with
    | Failure(x) -> let _print = Printf.printf "%s\n" x in None 
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
								Fun("z", Op(Sum, Var("z"), Op(Sum, Var("y"), Var("x")))), 
								Call(Var("sum_xyz"), Eint(5)),
							domains[3]), 
						domains[1]), 
					domains[0])
	|> Lib.Interpreter.sandbox_eval Lib.Env.emptyenv
	|> encapsulate_eval 
in List.map (expression) [permset_C; permset_A; permset_AC; permset_ABC]
	|> List.combine ["ABORT"; "ABORT"; "ABORT"; "OK"]
	|> List.iter format_test
;;