(* 
  Before sending into execution a mobile code, the user has the possibility to define some policies, which restrict 
    the execute itself.
*)
type my_policies = {
    mutable value: Policy.policy list;
}
;;
(* 
  Before sending into execution a mobile code, the user has the possibility to define some policies, which restrict 
    the execute itself.
*)
type my_domain = {
    mutable value: Code.domain;
}
;;
(* Initializing the user's policies *)
let (my_plc : my_policies) = {value=[]};;
(* Initializing the user's domain *)
let (my_dom : my_domain) = {value=Code.emptyDomain};;
(* Providing the user a way to add a list of policies to the policies already defined *)
let add_policy (p: Policy.policy list) = my_plc.value <- my_plc.value @ p
;;
(* Providing the user a way to remove a policy to the policies already defined *)
let remove_policy (p: Policy.policy) = my_plc.value <- List.filter (fun x -> x<>p) my_plc.value
;;
(* Providing the user a way to set the domain to which he belongs *)
let set_domain (d: Code.domain) = my_dom.value <- d
;;
(* Execute function evaluates an expression within a restricted contest e.g. a sandbox, with respect to
  the domain to which the user is assigned to and to the policies he wants to restrict the execution *)
let execute (exp: Ast.expr) = 
  try exp
    |> Interpreter.eval Interpreter.my_env [] my_plc.value true my_dom.value
    |> Option.some
  with 
    | Failure(x) -> let _print = Printf.printf "%s\n" x in None 
;;