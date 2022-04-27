type my_policies = {
    mutable value: Policy.policy list;
}
;;
type my_domain = {
    mutable value: Code.domain;
}
;;
let (my_plc : my_policies) = {value=[]};;
let (my_dom : my_domain) = {value=Code.emptyDomain};;
let add_policy (p: Policy.policy list) = my_plc.value <- my_plc.value @ p
;;
let remove_policy (p: Policy.policy) = my_plc.value <- List.filter (fun x -> x<>p) my_plc.value
;;
let set_domain (d: Code.domain) = my_dom.value <- d
;;
let execute (exp: Ast.expr) = 
  try exp
    |> Interpreter.eval Interpreter.my_env [] my_plc.value true my_dom.value
    |> Option.some
  with 
    | Failure(x) -> let _print = Printf.printf "%s\n" x in None 
;;