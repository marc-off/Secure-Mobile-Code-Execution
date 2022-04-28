open Lib
(* Policy : restricts the use of a write after a read in an expression *)
let (no_write_after_read : Policy.policy) = 
  let m_delta state event = 
    match state,event with
    | 0,Policy.Read -> Some 1 
    | 1, Policy.Write -> Some 2 
    | 0, Policy.Write -> Some 0 
    | 1,Policy.Read -> Some 1 
    | 2, _ -> Some 2 
    | n, Policy.Open -> Some n
    | n, Policy.Send -> Some n
    | _ -> failwith "Invalid transition"
  in 
  {
    states=[0;1;2];
    init_state=0;
    delta=m_delta;
    accept_states=[0;1]
  }
(* Policy : restricts the use of a open to only one occurrence in an expression *)
and (only_one_open : Policy.policy) = 
  let m_delta state event =  match state,event with 
    | 0,Policy.Open -> Some 1
    | 1, Policy.Open -> Some 2 
    | n, _ -> Some n 
  in 
  {
    states=[0;1;2];
    init_state=0;
    delta=m_delta;
    accept_states=[0;1]
  }
(* Policy : restricts the use of a write, denying every occurrence of it in an expression *)
and (no_write_allowed : Policy.policy) = 
  let m_delta state event =  match state,event with 
    | 0,Policy.Write -> Some 1
    | n, _ -> Some n 
  in 
  {
    states=[0;1;];
    init_state=0;
    delta=m_delta;
    accept_states=[0;]
  }
;;



(* Initializing the set of policies we want to restrict our 'execute' sandbox with *)
let () = Execute.add_policy ([only_one_open; no_write_after_read; no_write_allowed])	(* @ ... whatever policy we want to add *)
;;
(* Initializing the domain in which the 'execute' code is restricted to.
	Our permissions extracted from domains[1] will be the following: 
		R_perms = {B} , W_perms = {A} , O_perms = {A} , S_perms = {A,B} *)
let () = Execute.set_domain (List.nth Code.domains 1)
;;



let example1 = 
	(* First example in the homework, evaluating ((fun x = x+1)6) in the same domain SETTED BEFORE AT LINE 53 *)
	Ast.Let("fun",	Fun("x", Op(Sum, Var("x"),Eint(1))),	Call(Var("fun"), Eint(5)),	List.nth Code.domains 1)	
	|> Execute.execute
and example2 = (
	(* Second example in the homework TWEAKED, evaluating (let equal_5 x = (x=5)) and execute(let result = equal_5(5)) 
		in the same domain SETTED BEFORE AT LINE 53 *)
	let _e1 = Ast.Let("equal_5", Fun("x", Op(Equal, Var("x"), Eint(5))), Var("equal_5"), List.nth Code.domains 1)
	|> Interpreter.eval (Interpreter.my_env) [] [] false (List.nth Code.domains 1)
	in 
	Ast.Let("result", Call(Var("equal_5"), Eint(5)), Var("result"), List.nth Code.domains 1) 
	|> Execute.execute
)
and example3 = (
	(* Third example in the homework, evaluating (let mypin = 12345) and execute(let result = mypin in send(result)) 
		in the same domain SETTED BEFORE AT LINE 53 *)
	let _e2 = Ast.Let("mypin", Eint(12345), Var("mypin"), List.nth Code.domains 1)
	|> Interpreter.eval (Interpreter.my_env) [] [] false (List.nth Code.domains 1)
	in 
	Ast.Let("result", Var("mypin"), Send("result"), List.nth Code.domains 1)
	|> Execute.execute
)
in let _exec = 
	(* We label each value returned by the 'execute' provided by each example, with a tag "OK"/"ABORT" based on the expected result we assume to obtain *)
	List.combine ["OK"; "OK"; "OK"] [example1; example2; example3]
	(* We formalize the test, checking if the label "OK" corresponds to an effective successful computation -> Some v type value; 
		in the same way if the label "ABORT" corresopnds to an aborted computation -> None type value *)
	|> List.map Debug.format_test
(* This is the equivalent to the usual 'return 0' at the end of a C main *)
in true
;;