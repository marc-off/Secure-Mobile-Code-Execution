(* permset_A = {A} *)
let permset_A = 
	Lib.Code.PermSet.empty
	|> Lib.Code.PermSet.add "A"
;;
(* permset_B = {B} *)
let permset_B = 
	Lib.Code.PermSet.empty
	|> Lib.Code.PermSet.add "B"
;;
(* permset_C = {C} *)
let permset_C = 
	Lib.Code.PermSet.empty
	|> Lib.Code.PermSet.add "C"
;;
(* permset_AB = {A, B} *)
let permset_AB = 
	Lib.Code.PermSet.empty
	|> List.fold_right Lib.Code.PermSet.add ["A"; "B"]
;;
(* permset_BC = {B, C} *)
let permset_BC = 
	Lib.Code.PermSet.empty
	|> List.fold_right Lib.Code.PermSet.add ["B"; "C"]
;;
(* permset_AC = {A, C} *)
let permset_AC = 
	Lib.Code.PermSet.empty
	|> List.fold_right Lib.Code.PermSet.add ["A"; "C"]
;;
(* permset_ABC = {A, B, C} *)
let permset_ABC = 
	Lib.Code.PermSet.empty
	|> List.fold_right Lib.Code.PermSet.add ["A"; "B"; "C"]
;;
(* We define a set of domains, which represent the mobile code origin and especially the permissions
	 associated to them in the sandbox context. *)
let (domains: Lib.Code.domain list) = [
	{url = "https://www.ryanair.com/";
		read_perms = permset_A; write_perms = permset_C; open_perms = permset_C; send_perms = permset_AC};
	{url = "https://www.google.it/";
		read_perms = permset_B; write_perms = permset_A; open_perms = permset_A; send_perms = permset_AB};
	{url = "https://www.amazon.it/";
		read_perms = permset_C; write_perms = permset_AC; open_perms = permset_AC; send_perms = permset_BC};
	{url = "https://www.apple.com/it/";
		read_perms = permset_ABC; write_perms = permset_C; open_perms = permset_C; send_perms = permset_A};
	{url = "https://www.microsoft.com/";
		read_perms = permset_BC; write_perms = permset_A; open_perms = permset_A; send_perms = permset_A};
];;
(* Function for formatting the battery test, ensuring that the real output matches the expected one *)
let format_test tpair = match tpair with
	| (a, b) -> let _expected_res = Printf.printf "EXPECTED RES: %s - " a in match b with
		| None -> (
			if a="ABORT" then Printf.printf "OK TEST: execution aborted as expected!\n" 
				else Printf.printf "ERROR test (computation labelled as 'OK' returned no value)\n"
		)
		| Some v -> (
			if a="OK" then let ok_string = "OK TEST: the returned value is" in match v with
				| Lib.Ast.Int(n) -> Printf.printf "%s: %i\n" ok_string n
				| Lib.Ast.Bool(b) -> Printf.printf "%s: %B\n" ok_string b
				| Lib.Ast.Closure(id, _exp, _env) -> Printf.printf "%s the closure: %s\n" ok_string id
			else Printf.printf "ERROR test (computation labelled as 'ABORT' returned a value instead)\n"
		)
;;
(* Given an expression 'e' and some set of permissions 'p', it encapsulates the result of our interpreter.
	When the evaluation of our expression aborts ('failwith' case), it will return a None value, otherwise a Some 'v value. *)
let encapsulate_eval e =
  try 
		let encapsulate = Some (e) in let _print = Printf.printf "Permissions provided satisfied any check\n" in encapsulate
	with
    | Failure(x) -> let _print = Printf.printf "%s\n" x in None 
;;
(* 
	Expression translated in Ocaml syntax: let x = 2 in let y = 3 in let sum_xyz z = z + y + x in sum_xyz 5.
	Evaluation order is left-to-right, so when calling the function 'sum_xyz' we check perms for sum_xyz -> y -> x.
   At time of calling 'sum_xyz', Stack should be like this:
    |---Ide---|----Val----|--Perms--|
    |    x    |     2     |    A    |
    |    y    |     3     |    B    |
    | sum_xyz | z->x+y+z  |    C    |
    |---------|-----------|---------|
    Output expected should be 10 
*)
let expression = 
	(* Lib.Ast.Let("x", Eint(2), 
						Let("y", Eint(3), 
							Let("sum_xyz", 
								Fun("z", Op(Sum, Var("z"), Op(Sum, Var("y"), Var("x")))), 
								Call(Var("sum_xyz"), Eint(5)),
							List.nth domains 2), 
						List.nth domains 1), 
					List.nth domains 0) *)
	Lib.Ast.Let("x", 
				Eint(2), 
				Let("y", 
					Var("x"), 
					Let("sum",
						Fun("z", Op(Sum, Var("z"), Var("y"))),
						Let(
							"x", 
							Eint(6), 
							Call(Var("sum"), Eint(5)), 
							List.nth domains 0
						),
						List.nth domains 0
					), 
					List.nth domains 0
				), 
				List.nth domains 0
			) 
(*
	We call now the 'eval' function, given the expression before, restricted to the semantics of the defined sandbox.
*)
	|> Lib.Interpreter.eval Lib.Env.emptyenv [] [] true (List.nth domains 1)
(* 
	The sandboxed 'eval' is now given as input to the defined funtion 'encapsulate_eval', which encapsulates the 
	returned value and handles the 'failwith' calls, which in turn return the 'Failure' type value.
*)
	|> encapsulate_eval
in [("OK", expression)]
	|> List.iter format_test
;;

(* 
	The 'encapsulate_eval' function now is an input of the List.map function, which we remember to act in this way:
	List.map f [a1;a2;a3;…;an] -> [f a1;f a2;f a3;…;f an].
	The only missing argument for 'encapsulate_eval' is the permissions required for the 'sandbox_eval', which
	are indeed in the last argument of the List.map function.
*)
(* in let _exec_test = List.map (expression) [List.nth domains 2; List.nth domains 1; List.nth domains 4; List.nth domains 3] *)
(* 
	Since the 'encapsulate_eval' returns either a None type value or a Some type value, we will obtain a list 
	of None-Some type values. Now, we combine each element with another element, obtaining a list of pairs
	where the first is a 'label' which is the expected result of the test cases, and the second is a None/Some value type
*)
	(* |> List.combine ["ABORT"; "ABORT"; "ABORT"; "OK"] *)
(* 
	Finally, we format the pairs in way that we can print out the results of our test.
*)
	(* |> List.iter format_test *)
(* This is the equivalent to the usual 'return 0' at the end of a C main *)
(* in true *)
;;

(* let get_result exp =
  try
    exp
    |> eval [] [] []
    |> Option.some
  with
    Failure _ -> None

let only_one_open =
  let m_delta state event =  match state,event with
    | 0, Policy.Open -> Some 1
    | 1, Policy.Open -> Some 2
    | n, _ -> Some n
  in
  {
    Policy.states=[0;1;2];
    init_state=0;
    delta=m_delta;
    accept_states=[0;1]

  } *)

