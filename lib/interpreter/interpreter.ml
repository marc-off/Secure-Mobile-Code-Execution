open Ast
(* Instantiation of the local environment, which saves the bindings from each evaluation of an expression *)
let my_env : Ast.value Env.env = {state=[]}
;;
(* 
  The definition of our eval function has been tweaked with respect to the new features of sandboxing.

  - We still take as parameters both trace of events and the policies restricting the evaluation.
  - We take also a 'sandbox' flag: when 'true', takes in account the checking of permissions for accessing resources.
  - It also takes the 'domain' of the current code, which will be assigned to the local bindings and, eventually, compared for checking of permissions.
 *)
let rec eval
            (env : Ast.value Env.env)
            (trace : Policy.event list) 
            (policies : Policy.policy list) 
            (sandbox : bool) 
            (domain : Code.domain)
            (exp : Ast. expr)  =
  match exp with
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | Eint(n) -> Int(n)
  | Var(i) -> if sandbox then i |> Env.lookup_sandboxed env domain else i |> Env.lookup_root env 
  | Op(o, m1, m2) -> (
      let v1 = m1 |> eval env trace policies sandbox domain in
      let v2 = m2 |> eval env trace policies sandbox domain in
        match (o , v1, v2) with
        | Sum, Int(i1), Int(i2) -> Int(i1+i2) 
        | Minus, Int(i1), Int(i2) -> Int(i1-i2)
        | Times, Int(i1), Int(i2) -> Int(i1*i2)
        | Equal, Int(i1), Int(i2) -> Bool(i1=i2)
        | Less, Int(i1), Int(i2) -> Bool(i1<i2)
        | Greater, Int(i1), Int(i2) -> Bool(i1>i2)
        | _, _, _ -> failwith ("Pattern matching of Op not recognized"))
  |Let(id, e1, e2, d) -> (
    let new_val = e1 |> eval env trace policies sandbox domain in
    let add_trace = Policy.get_trace e1 in
    let _bind = (if sandbox then ignore(0) else (id, new_val, d) |> Env.bind_local my_env) in
    e2 |> eval ((id, new_val, d) |> Env.bind_temp env) (trace @ add_trace) policies sandbox domain)
  |If(g, e1, e2) -> (
    let guard = g |> eval env trace policies sandbox domain in
    match guard with
    | Bool(true) -> e1 |> eval env trace policies sandbox domain
    | Bool(false) -> e2 |> eval env trace policies sandbox domain
    | _ -> failwith "Evaluation of If-guard lead to uncompatible type!")
  |Fun(id, e) -> Closure(id, e, env)
  |Call(f, arg) -> (
    let value_f = f |> eval env trace policies sandbox domain in
    let value_arg = arg |> eval env trace policies sandbox domain in
      match value_f with
      | Closure(param, body, env) -> (
        let add_t = Policy.get_trace arg in
        body |> eval ((param, value_arg, domain) |> Env.bind_temp env) (trace @ add_t) policies sandbox domain)
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


