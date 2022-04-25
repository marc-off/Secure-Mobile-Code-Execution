open Ast

(* Evaluation of expression abstracting from the semantics of permissions *)
let rec eval (env : Ast.value Env.env) (exp : expr) = 
  match exp with
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | Eint(n) -> Int(n)
    | Var(i) -> Env.lookup_root env i 
    | Op(o, m1, m2) -> (
      let v1 = eval env m1 in
      let v2 = eval env m2 in
      match (o, v1, v2) with
				| Sum, Int(i1), Int(i2) -> Int(i1+i2)
				| Minus, Int(i1), Int(i2) -> Int(i1-i2)
				| Times, Int(i1), Int(i2) -> Int(i1*i2)
				| Equal, Int(i1), Int(i2) -> Bool(i1=i2)
				| Less, Int(i1), Int(i2) -> Bool(i1<i2)
				| Greater, Int(i1), Int(i2) -> Bool(i1>i2)
				| _, _, _ -> failwith ("Pattern matching of Op not recognized"))
    | Let(id, e1, e2, p) -> (
      let new_val = eval env e1 in
      let new_env = Env.bind env id new_val p in
        eval new_env e2)
    | If(g, e1, e2) -> (
      let guard = eval env g in
        match guard with
        | Bool(true) -> eval env e1
        | Bool(false) -> eval env e2
        | _ -> failwith "Evaluation of If lead to uncompatible type!")
    | Fun (id, e) -> Closure(id, e, env)
    | Call (f, arg) -> (
      let value_f = eval env f in
      (* The evaluation of arg must take place inside p_stack
      before extending the stack *)
      let value_arg = eval env arg in 
      match value_f with
        | Closure (param, body, closure_env) ->
          (* Extend the environment by assigning the argument to the parameter name *) 
          let new_env = Env.bind closure_env param value_arg Env.PermSet.empty in
          eval new_env body
        | _ -> failwith "A function is required on the left side of the call.")
;;
(* Evaluation of an expression restricted to the notion of sanbox *)
let rec sandbox_eval (env : Ast.value Env.env) (exp : expr) (perms : Env.PermSet.t) = 
  match exp with 
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | Eint(n) -> Int(n)
    | Var(i) -> Env.lookup_sandboxed env i perms
    | Op(o, m1, m2) -> (
      let v1 = sandbox_eval env m1 perms in
      let v2 = sandbox_eval env m2 perms in
        match (o, v1, v2) with
        | Sum, Int(i1), Int(i2) -> Int(i1+i2)
        | Minus, Int(i1), Int(i2) -> Int(i1-i2)
        | Times, Int(i1), Int(i2) -> Int(i1*i2)
        | Equal, Int(i1), Int(i2) -> Bool(i1=i2)
        | Less, Int(i1), Int(i2) -> Bool(i1<i2)
        | Greater, Int(i1), Int(i2) -> Bool(i1>i2)
        | _, _, _ -> failwith ("Pattern matching of Op not recognized"))
    | Let(id, e1, e2, p) -> (
      let new_val = sandbox_eval env e1 perms in
      let new_env = Env.bind env id new_val p in
        sandbox_eval new_env e2 perms)
    | If(g, e1, e2) -> (
      let guard = sandbox_eval env g perms in
        match guard with
        | Bool(true) -> sandbox_eval env e1 perms
        | Bool(false) -> sandbox_eval env e2 perms
        | _ -> failwith "Evaluation of If lead to uncompatible type!")
    | Fun (id, e) -> Closure(id, e, env)
    | Call (f, arg) -> (
    let value_f = sandbox_eval env f perms in
    (* The evaluation of arg must take place inside p_stack
    before extending the stack *)
    let value_arg = sandbox_eval env arg perms in 
    match value_f with
      | Closure (param, body, closure_env) ->
        (* Extend the environment by assigning the argument to the parameter name *) 
        let new_env = Env.bind closure_env param value_arg Env.PermSet.empty in
        sandbox_eval new_env body perms
      | _ -> failwith "A function is required on the left side of the call.")
;;
