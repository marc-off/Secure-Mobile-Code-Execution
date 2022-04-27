open Ast
(*  *)
let get_trace expr =
  match expr with
  | Read _ -> [Policy.Read]
  | Write _ -> [Policy.Write]
  | Open _ -> [Policy.Open]
  | Send _ -> [Policy.Send]
  | _ -> []
;;
(*  *)
let my_env : Ast.value Env.env = {state=[]}
;; 
(*
The following changes are made to eval to accomodate for the new security policy:

  - we take as parameter the trace of events and the list of security policies to check
  - the trace of events is used to keep track of relevant actions to check in security policies
  - Read, Write and open operations are checked to ensure they do not violate security policies. The check consists of evaluating the automata with the current execution trace.
  This is a very simple but inefficient way to check them, since the automata gets entirely rerun at every operation. An optimization would be to inline the automatas as code directly,
  adding variables and code for their native execution and failure handling
 *)
let rec eval 
            (trace : Policy.event list) 
            (policies : Policy.policy list) 
            (sandbox : bool) 
            (domain : Code.domain)
            (exp : expr)  =
  match exp with
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | Eint(n) -> Int(n)
  | Var(i) -> if sandbox then i |> Env.lookup_sandboxed my_env domain else i |> Env.lookup_root my_env 
  | Op(o, m1, m2) -> (
      let v1 = m1 |> eval trace policies sandbox domain in
      let v2 = m2 |> eval trace policies sandbox domain in
        match (o , v1, v2) with
        | Sum, Int(i1), Int(i2) -> Int(i1+i2)
        | Minus, Int(i1), Int(i2) -> Int(i1-i2)
        | Times, Int(i1), Int(i2) -> Int(i1*i2)
        | Equal, Int(i1), Int(i2) -> Bool(i1=i2)
        | Less, Int(i1), Int(i2) -> Bool(i1<i2)
        | Greater, Int(i1), Int(i2) -> Bool(i1>i2)
        | _, _, _ -> failwith ("Pattern matching of Op not recognized"))
  |Let(id, e1, e2, d) -> (
    let new_val = e1 |> eval trace policies sandbox domain in
    let add_trace = get_trace e1 in
    let _bind = (id, new_val, d) |> Env.bind my_env in  
    e2 |> eval (trace @ add_trace) policies sandbox domain)
  |If(g, e1, e2) -> (
    let guard = g |> eval trace policies sandbox domain in
    match guard with
    | Bool(true) -> e1 |> eval trace policies sandbox domain
    | Bool(false) -> e2 |> eval trace policies sandbox domain
    | _ -> failwith "Evaluation of If-guard lead to uncompatible type!")
  |Fun(id, e) -> Closure(id, e, my_env)
  |Call(f, arg) -> (
    let value_f = f |> eval trace policies sandbox domain in
    let value_arg = arg |> eval trace policies sandbox domain in
      match value_f with
      | Closure(param, body, env) -> (
        let add_t = get_trace arg in
        let stack = my_env.state <- env.state @ my_env.state in
        let _bind = (param, value_arg, domain) |> Env.bind (stack) in
        body |> eval (trace @ add_t) policies sandbox domain)
      | _ -> failwith "A function is required on the left side of the call.")
  (* Read Write Open does not do any concrete operation to simplify reasoning *)
  | Read(id) -> 
    if (Policy.check_policies (trace @ [Policy.Read]) policies) then 
      if sandbox then
        let ret_value = Env.check_perms my_env id domain Read in
          match ret_value with
          | bool, msg -> let _print = Printf.printf "%s\n" msg in Bool(bool)
      else Bool(true)
    else failwith "Invalid read"
  | Write(id) -> 
    if (Policy.check_policies (trace @ [Policy.Write]) policies) then
      if sandbox then
        let ret_value = Env.check_perms my_env id domain Write in
          match ret_value with
          | bool, msg -> let _print = Printf.printf "%s\n" msg in Bool(bool)
      else Bool(true)
    else failwith "Invalid write"
  | Open(id) -> 
    if (Policy.check_policies (trace @ [Policy.Open]) policies) then 
      if sandbox then
        let ret_value = Env.check_perms my_env id domain Open in
          match ret_value with
          | bool, msg -> let _print = Printf.printf "%s\n" msg in Bool(bool)
      else Bool(true)
    else failwith "Invalid open"
  | Send(id) -> 
    if (Policy.check_policies (trace @ [Policy.Send]) policies) then 
      if sandbox then
        let ret_value = Env.check_perms my_env id domain Send in
          match ret_value with
          | bool, msg -> let _print = Printf.printf "%s\n" msg in Bool(bool)
      else Bool(true)
    else failwith "Invalid send"
;;

