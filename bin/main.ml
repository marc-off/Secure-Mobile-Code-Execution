open Lib
open Printf
(* Function for formatting the battery test, ensuring that the real output matches the expected one *)
let _format_test tpair = match tpair with
	| (a, b) -> let _expected_res = printf "EXPECTED RES: %s - " a in match b with
		| None -> (
			if a="ABORT" then printf "OK TEST: execution aborted as expected!\n" 
				else printf "ERROR test (computation labelled as 'OK' returned no value)\n"
		)
		| Some v -> (
			if a="OK" then let ok_string = "OK TEST: the returned value is" in match v with
				| Ast.Int(n) -> printf "%s: %i\n" ok_string n
				| Ast.Bool(b) -> printf "%s: %B\n" ok_string b
				| Ast.Closure(id, _exp, _env) -> printf "%s the closure: %s\n" ok_string id
			else printf "ERROR test (computation labelled as 'ABORT' returned a value instead)\n"
		)
;;

let _no_write_after_read = 
  let m_delta state event = 
    match state,event with
    | 0,Policy.Read -> Some 1 
    | 1, Policy.Write -> Some 2 
    | 0, Policy.Write -> Some 0 
    | 1,Policy.Read -> Some 1 
    | 2, _ -> Some 2 
    | n, Policy.Open -> Some n
    | _ -> failwith "Invalid transition"
  in Execute.add_policy
  [{
    states=[0;1;2];
    init_state=0;
    delta=m_delta;
    accept_states=[0;1]

  }]
;;
let _only_one_open = 
  let m_delta state event =  match state,event with 
    | 0,Policy.Open -> Some 1
    | 1, Policy.Open -> Some 2 
    | n, _ -> Some n 
  in Execute.add_policy
  [{
    states=[0;1;2];
    init_state=0;
    delta=m_delta;
    accept_states=[0;1]

  }]
;;
let _no_write_allowed = 
  let m_delta state event =  match state,event with 
    | 0,Policy.Write -> Some 1
    | n, _ -> Some n 
  in Execute.add_policy
  [{
    states=[0;1;];
    init_state=0;
    delta=m_delta;
    accept_states=[0;]

  }]
;;
let _setdom = Execute.set_domain (List.nth Code.domains 1)
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
let _bindrandom = 
	Ast.Let("x", Eint(2), Var("x"), List.nth Code.domains 0)
	|> Interpreter.eval (Interpreter.my_env) [] [] false (List.nth Code.domains 3)
;;
let _a = Debug.format_env Interpreter.my_env
;;
let _callexec = 
	Ast.Var("x")
	|> Execute.execute
;; 
(* ;;
let _expressions = [
	(* BAD *)
	(Ast.Let("x", Eint(2), 
				Let("y", Eint(3), 
					Let("sum_xyz", Fun("z", Op(Sum, Var("z"), Op(Sum, Var("y"), Var("x")))), Call(Var("sum_xyz"), Eint(5)),
					List.nth Code.domains 2), 
				List.nth Code.domains 1), 
			List.nth Code.domains 0)
	);
	(* GOOD *)
	(Ast.Let("x", Eint(2), 
						Let("y", Var("x"), 
							Let("sum", Fun("z", Op(Sum, Var("z"), Var("y"))),
								Let("x", Eint(6), Call(Var("sum"), Eint(5)), 
								List.nth Code.domains 0),
							List.nth Code.domains 0), 
					List.nth Code.domains 0), 
				List.nth Code.domains 0)
	);
	(* GOOD *)
	(Ast.Let("add_3",Fun("n",Op(Sum,Var("n"),Eint(3))),Call(Var("add_3"),Eint(5)),
						List.nth Code.domains 0)
	);
	(* BAD: policy *)
  (Ast.Let("x",Read("f"),Write("f"),
						List.nth Code.domains 0)
	);
  (* BAD: policy *)
  (Ast.Let("x",Write("f"),Read("f"),
					List.nth Code.domains 2)
	);
  (* BAD: policy *)
  (Ast.Let("x",Eint(2),Write("f"),
					List.nth Code.domains 3)
	);
  (* GOOD *)
  (Ast.Let("x",Eint(2),Read("f"),
					List.nth Code.domains 3)
	);
	(* BAD: policy *)
  (Ast.Let("x",Open("f"),Open("f2"),
					List.nth Code.domains 1)
	);
  (* OK *)
  (Ast.Let("x",Open("f"),Read("f"),
					List.nth Code.domains 0)
	)
]
(*
	We call now the 'eval' function, given the expression before, restricted to the semantics of the defined sandbox.
*)
	|> List.map Execute.execute 
	(* We assign to each round of execute (returning a Some/None type value) a label, which is the expected result
		"OK" -> Some value
		"ABORT" -> None *)
	|> List.combine ["ABORT"; "OK"; "OK"; "ABORT"; "ABORT"; "ABORT"; "OK"; "ABORT"; "OK"]

	|> List.map format_test
;;

(* 
	The sandboxed 'eval' is now given as input to the defined funtion 'encapsulate_eval', which encapsulates the 
	returned value and handles the 'failwith' calls, which in turn return the 'Failure' type value.
*)
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
in true *)
