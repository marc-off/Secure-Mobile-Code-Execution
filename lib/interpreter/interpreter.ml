open Ast
let local_env = Lib.Env.empty;
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
    | Let(id, e1, e2, _d) -> (
      let new_val = eval env e1 in
      let new_env = Env.bind env id new_val Code.emptyDomain in
        eval new_env e2)
    | If(g, e1, e2) -> (
      let guard = eval env g in
        match guard with
        | Bool(true) -> eval env e1
        | Bool(false) -> eval env e2
        | _ -> failwith "Evaluation of If-guard lead to uncompatible type!")
    | Fun (id, e) -> Closure(id, e, env)
    | Call (f, arg) -> (
      let value_f = eval env f in
      (* The evaluation of arg must take place inside p_stack
      before extending the stack *)
      let value_arg = eval env arg in
      match value_f with
        | Closure (param, body, closure_env) ->
          (* Extend the environment by assigning the argument to the parameter name *)
          let new_env = Env.bind closure_env param value_arg Code.emptyDomain in
          eval new_env body
        | _ -> failwith "A function is required on the left side of the call.")
    | Read (_id) -> Bool(true)
    | Write (_id) -> Bool(true)
    | Send (_id) -> Bool(true)
;;
(* Evaluation of an expression restricted to the notion of sanbox *)
let rec sandbox_eval (env : Ast.value Env.env) (exp : expr) (domain : Code.domain) =
  match exp with
    | CstTrue -> Bool(true)
    | CstFalse -> Bool(false)
    | Eint(n) -> Int(n)
    | Var(i) -> Env.lookup_sandboxed env i domain
    | Op(o, m1, m2) -> (
      let v1 = sandbox_eval env m1 domain in
      let v2 = sandbox_eval env m2 domain in
        match (o, v1, v2) with
        | Sum, Int(i1), Int(i2) -> Int(i1+i2)
        | Minus, Int(i1), Int(i2) -> Int(i1-i2)
        | Times, Int(i1), Int(i2) -> Int(i1*i2)
        | Equal, Int(i1), Int(i2) -> Bool(i1=i2)
        | Less, Int(i1), Int(i2) -> Bool(i1<i2)
        | Greater, Int(i1), Int(i2) -> Bool(i1>i2)
        | _, _, _ -> failwith ("Pattern matching of Op not recognized"))
    | Let(id, e1, e2, d) -> (
      let new_val = sandbox_eval env e1 d in
      let new_env = Env.bind env id new_val d in
        sandbox_eval new_env e2 domain)
    | If(g, e1, e2) -> (
      let guard = sandbox_eval env g domain in
        match guard with
        | Bool(true) -> sandbox_eval env e1 domain
        | Bool(false) -> sandbox_eval env e2 domain
        | _ -> failwith "Evaluation of If lead to uncompatible type!")
    | Fun (id, e) -> Closure(id, e, env)
    | Call (f, arg) -> (
    let value_f = sandbox_eval env f domain in
    (* The evaluation of arg must take place inside p_stack
    before extending the stack *)
    let value_arg = sandbox_eval env arg domain in
    match value_f with
      | Closure (param, body, closure_env) ->
        (* Extend the environment by assigning the argument to the parameter name *)
        let new_env = Env.bind closure_env param value_arg Code.emptyDomain in
        sandbox_eval new_env body domain
      | _ -> failwith "A function is required on the left side of the call.")
    | Read (id) -> (let ret_value = Env.check_perms env id domain Read in
      match ret_value with
      | bool, msg -> let _print = Printf.printf "%s\n" msg in Bool(bool)
    )
    | Write (id) -> (let ret_value = Env.check_perms env id domain Write in
      match ret_value with
      | bool, msg -> let _print = Printf.printf "%s\n" msg in Bool(bool)
    )
    | Send (id) -> (let ret_value = Env.check_perms env id domain Send in
      match ret_value with
      | bool, msg -> let _print = Printf.printf "%s\n" msg in Bool(bool)
    )
;;


(* ---------------- *)
(* ---------------- *)
(* ---------------- *)
(* ---------------- *)
(* ---------------- *)

(* Interpreter *)

(*exp as last argument in order to exploit partial function application to use the |> operator*)

open Ast

let get_trace expr =
  (* helper function to associate relevant expressions to the list of events that they produce *)
  match expr with
  | Read _ -> [Lib.Policy.Read]
  | Write _ -> [Lib.Policy.Write]
  | Open _ -> [Lib.Policy.Open]
  | Send _ -> [Lib.Policy.Send]
  | _ -> []


(*
The following changes are made to eval to accomodate for the new security policy:

  - we take as parameter the trace of events and the list of security policies to check
  - the trace of events is used to keep track of relevant actions to check in security policies
  - Read, Write and open operations are checked to ensure they do not violate security policies. The check consists of evaluating the automata with the current execution trace.
  This is a very simple but inefficient way to check them, since the automata gets entirely rerun at every operation. An optimization would be to inline the automatas as code directly,
  adding variables and code for their native execution and failure handling
 *)

let rec eval (env: Ast.value Env.env) (trace: Security_policy.event list) (policies: Security_policy.policy list) (sandbox: bool) (expr:exp)  =
  match expr with
  |Eint(n) -> Int n
  |Ebool b -> Bool b
  |Fun(x, body) -> Closure(x,body,env)
  |Den x -> if sandbox then x |> Env.lookup env else x CON PERMESSI |> Env.lookup local_env CON PERMESSI
  |If(e1,e2,e3) ->
    (match e1 |> eval env trace policies sandbox with
     | Bool true -> eval env trace policies sandbox e2
     | Bool false -> eval env trace policies sandbox e3
     | _ -> failwith "If guard must evaluate to a boolean")
  |Let(id, e1, e2) -> let v1 = e1 |> eval env trace policies sandbox in
    let add_t = get_trace e1 in
    e2 |> eval ((id,v1)|> if sandbox then Env.bind env else Env.bind local_env) (trace @ add_t) policies sandbox
  |Binop(op,e1,e2) -> (
      let e1 = eval env trace policies sandbox e1 in
      let e2 = eval env trace policies sandbox e2 in
      match op,e1,e2 with
      | Sum,Int n1, Int n2 -> Int (n1+n2)
      | Times,Int n1, Int n2 -> Int (n1 * n2)
      | Minus,Int n1, Int n2 -> Int (n1-n2)
      | Equal, Int n1, Int n2 -> Bool (n1 = n2)
      | Less, Int n1, Int n2 -> Bool (n1 < n2)
      | Greater, Int n1, Int n2 -> Bool (n1 > n2)
      | _ -> failwith "Invalid binary expression")
  |Call(f, arg) -> (match eval env trace policies sandbox f with
      | Closure(x,body,env) -> (let x_val = arg |> eval env trace policies sandbox in
        let add_t = get_trace arg in
          body |> eval ((x, x_val) |> if sandbox then Env.bind env else Env.bind local_env) (trace @ add_t) policies)
      | _ -> failwith "Not calling a function")
  (* Read Write Open does not do any concrete operation to simplify reasoning *)
  | Read x -> if (Lib.Policy.check_policies (trace @ [Security_policy.Read]) policies) then Bool true
                  else failwith "Invalid read"
  | Write x -> if (Lib.Policy.check_policies (trace @ [Security_policy.Write]) policies) then Bool true
                  else failwith "Invalid write"
  | Open x -> if (Lib.Policy.check_policies (trace @ [Security_policy.Open]) policies) then Bool true 
                  else failwith "Invalid open"
  | Send x -> if (Lib.Policy.check_policies (trace @ [Security_policy.Send]) policies) then Bool true
                  else failwith "Invalid send"

(* Main *)

let get_result exp =
  try
    exp
    |> Interpreter.eval local_env [] []
    |> Option.some
  with
    Failure _ -> None

let only_one_open =
  let m_delta state event =  match state,event with
    | 0,Security_policy.Open -> Some 1
    | 1, Security_policy.Open -> Some 2
    | n, _ -> Some n
  in
  {
    states=[0;1;2];
    init_state=0;
    delta=m_delta;
    accept_states=[0;1]

  }

eval(let mysum = (fun x -> (fun y -> x + y)));;
local_env= [mysum; x -> y -> x + y; A];;
execute -> eval exp: let result = mysum(5, 5) s: true env: []
