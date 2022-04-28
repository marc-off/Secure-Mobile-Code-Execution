open Code
(* 
  An environment is characterized by 3-tuples of an identifier, a generic type 'v and a domain which restricts accesses to the variable.
  The implementation of the type 'env' has been changed to accomodate the definition of a local environment, where the bindings are not 
    restricted to the evaluation of an expression, but remain as references for successive 'execute' calls (see example in the consegna).
*)
type 'v env = {
  mutable state : (string * 'v * Code.domain) list 
}
;;
(* Semantic of lookup in an environment abstracting from notion of domain, as if ran with root perms *)
let rec lookup_root (e: 'v env) (x: string) =
  match e.state with 
    | [] -> failwith "Binding not found!"
    | (ide, value, _domain)::r -> if x = ide then value else lookup_root {state=r} x
;;
(* Semantic of lookup restricted to the definition of a domain for each resource in the stack – note that lookup counts as a read op! *)
let rec lookup_sandboxed (e:'v env) (d: Code.domain) (x: string) = 
  match e.state with 
  | [] -> failwith "Binding not found"
  | (ide, value, domain)::r -> if x = ide then 
      (* Reminder that, from the Set library, Set.subset s1 s2 returns true iff s1 is a subset of s2 *)
      if Code.PermSet.subset domain.read_perms d.read_perms then value else failwith ("Unsufficient permissions for " ^ide)
    else lookup_sandboxed {state=r} d x
;;
(* Checkup for the permissions of a certain resource in the stack, returning state of the exit + message *)
let rec check_perms (e:'v env) (x: string) (d: Code.domain) (prim_op: Code.primitive) = 
  match e.state with 
  | [] -> (false, "Resource "^x^" not found in the stack")
  | (ide, _value, domain)::r -> if x = ide then match prim_op with
    | Read -> if Code.PermSet.subset domain.read_perms d.read_perms then (true, "Read OK") 
              else (false, "Not enough permissions for reading resource "^ide)
    | Write -> if Code.PermSet.subset domain.write_perms d.write_perms then (true, "Write OK") 
              else (false, "Not enough permissions for writing resource "^ide)
    | Open -> if Code.PermSet.subset domain.open_perms d.open_perms then (true, "Open OK") 
              else (false, "Not enough permissions for opening resource "^ide)
    | Send -> if Code.PermSet.subset domain.send_perms d.send_perms then (true, "Send OK") 
              else (false, "Not enough permissions for sending resource "^ide)
  else check_perms {state=r} x d prim_op
;;
(* We bind the variable to a value and a domain limiting its access. 
  Returns the environment passed as argument, with the new binding *)
let bind_local (env:'v env) (x, v, d) = env.state <- (x,v,d)::env.state
;;
(* We bind the variable to a value and a domain limiting its access. 
  Returns a COPY of the environment passed as argument, with the new binding in the COPY, without altering the original state. *)
let bind_temp (env:'v env) (x, v, d) = {state=(x,v,d)::env.state}
;;