open Ast
open Code
open Env

(* Evaluation of expression abstracting from the semantics of permissions *)
let rec eval 

(* Evaluation of an expression restricted to the notion of sanbox *)
let rec sandbox_eval (exp : expr) (perms : Env.PermSet.t) (env : Ast.value Env.env) (sandbox_flag :)= 
    match exp with 
      | CstTrue -> Bool(true)
      | CstFalse -> Bool(false)
      | Eint(n) -> Int(n)
      | Var(i) -> Env.lookup env i perms
      | Op(o, m1, m2) -> 
        let v1 = eval m1 dom env in
        let v2 = eval m2 dom env in
          match (o, v1, v2) with
          | Sum, Int(i1), Int(i2) -> Int(i1+i2)
          | Minus, Int(i1), Int(i2) -> Int(i1-i2)
          | Times, Int(i1), Int(i2) -> Int(i1*i2)
          | Equal, Int(i1), Int(i2) -> Bool(i1=i2)
          | Less, Int(i1), Int(i2) -> Bool(i1<i2)
          | Greater, Int(i1), Int(i2) -> Bool(i1>i2)
          | _, _, _ -> failwith ("Pattern matching of Op not recognized")
      | Let(id, e1, e2) -> 
        let new_val = eval e1 dom env in
        let new_env = Env.bind env id new_val  in
          eval e2 dom new_env
      | If(g, e1, e2) -> 
        let guard = eval g dom env in
          match guard with
          | Bool(true) -> eval e1 dom env
          | Bool(false) -> eval e2 dom env
          | _ -> failwith "Evaluation of If lead to uncompatible type!"
      | Fun(param, body, domain) -> 
        if Env.PermSet.subset domain.r_perms perms then Closure(param, body, domain, env)
        else failwith "Unsufficient permissions for function in domain"
      | Call(f, arg, domain) -> 
        let new_perms = Env.PermSet.inter domain.r_perms perms in
        let value_f = eval f new_perms env in
        let value_arg = eval arg perms env in
        match value_f with
          | Closure(param, body, domain, env) -> 
            let new_env = Env.bind env param value_arg domain.r_perms in
              eval body 
      | Read -> 
      | Write ->
      | Send ->
      | Delete ->
;;

let execute (e : expr) (d : Code.domain) = 
;;