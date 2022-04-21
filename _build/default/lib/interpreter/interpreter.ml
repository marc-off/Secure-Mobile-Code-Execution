open Ast

(* Evaluation of expression abstracting from the semantics of permissions *)
let rec eval (exp : expr) (env : Ast.value Env.env) = 
  match exp with
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | Eint(n) -> Int(n)
    | Var(i) -> Env.lookup_root env i 
    | Op(o, m1, m2) -> (
      let v1 = eval m1 env in
      let v2 = eval m2 env in
      match (o, v1, v2) with
				| Sum, Int(i1), Int(i2) -> Int(i1+i2)
				| Minus, Int(i1), Int(i2) -> Int(i1-i2)
				| Times, Int(i1), Int(i2) -> Int(i1*i2)
				| Equal, Int(i1), Int(i2) -> Bool(i1=i2)
				| Less, Int(i1), Int(i2) -> Bool(i1<i2)
				| Greater, Int(i1), Int(i2) -> Bool(i1>i2)
				| _, _, _ -> failwith ("Pattern matching of Op not recognized"))
    | Let(id, e1, e2, p) -> (
      let new_val = eval e1 env in
      let new_env = Env.bind env id new_val p in
        eval e2 new_env)
    | If(g, e1, e2) -> (
      let guard = eval g env in
        match guard with
        | Bool(true) -> eval e1 env
        | Bool(false) -> eval e2 env
        | _ -> failwith "Evaluation of If lead to uncompatible type!")
    | Fun (id, e) -> Closure(id, e, env)
    | Call (f, arg) -> (
      let value_f = eval f env in
      (* The evaluation of arg must take place inside p_stack
      before extending the stack *)
      let value_arg = eval arg env in 
      match value_f with
        | Closure (param, body, closure_env) ->
          (* Extend the environment by assigning the argument to the parameter name *) 
          let new_env = Env.bind closure_env param value_arg Env.PermSet.empty in
          eval body new_env
        | _ -> failwith "A function is required on the left side of the call.")
;;
(* Evaluation of an expression restricted to the notion of sanbox *)
let rec sandbox_eval (exp : expr) (env : Ast.value Env.env) (perms : Env.PermSet.t) = 
  match exp with 
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | Eint(n) -> Int(n)
    | Var(i) -> Env.lookup_sandboxed env i perms
    | Op(o, m1, m2) -> (
      let v1 = sandbox_eval m1 env perms in
      let v2 = sandbox_eval m2 env perms in
        match (o, v1, v2) with
        | Sum, Int(i1), Int(i2) -> Int(i1+i2)
        | Minus, Int(i1), Int(i2) -> Int(i1-i2)
        | Times, Int(i1), Int(i2) -> Int(i1*i2)
        | Equal, Int(i1), Int(i2) -> Bool(i1=i2)
        | Less, Int(i1), Int(i2) -> Bool(i1<i2)
        | Greater, Int(i1), Int(i2) -> Bool(i1>i2)
        | _, _, _ -> failwith ("Pattern matching of Op not recognized"))
    | Let(id, e1, e2, p) -> (
      let new_val = sandbox_eval e1 env perms in
      let new_env = Env.bind env id new_val p in
        sandbox_eval e2 new_env perms)
    | If(g, e1, e2) -> (
      let guard = sandbox_eval g env perms in
        match guard with
        | Bool(true) -> sandbox_eval e1 env perms
        | Bool(false) -> sandbox_eval e2 env perms
        | _ -> failwith "Evaluation of If lead to uncompatible type!")
    | Fun (id, e) -> Closure(id, e, env)
    | Call (f, arg) -> (
    let value_f = sandbox_eval f env perms in
    (* The evaluation of arg must take place inside p_stack
    before extending the stack *)
    let value_arg = sandbox_eval arg env perms in 
    match value_f with
      | Closure (param, body, closure_env) ->
        (* Extend the environment by assigning the argument to the parameter name *) 
        let new_env = Env.bind closure_env param value_arg Env.PermSet.empty in
        sandbox_eval body new_env perms
      | _ -> failwith "A function is required on the left side of the call.")
;;
