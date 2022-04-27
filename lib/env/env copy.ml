open Code
(* An environment is characterized by an identifier, a generic type 'v and a domain which restricts accesses *)
type 'v env = (string * 'v * Code.domain) list 
;;
(* A local environment which is of mutable type, implying that all changes between different 'eval' calls are saved *)
type 'v local_env = {
  mutable state: 'v env;
}
;;
(* Initializing the environment, which is set to empty *)
let emptyenv = []
;;
(* Semantic of lookup abstracting from notion of domain, as if ran with root perms *)
let rec lookup_root (e: 'v env) (x: string) =
  match e with 
    | [] -> failwith "Binding not found!"
    | (ide, value, _domain)::r -> if x = ide then value else lookup_root r x
;;
(* Semantic of lookup restricted to the definition of a domain for each resource in the stack – note that lookup and read are the same op! *)
let rec lookup_sandboxed (e:'v env) (d: Code.domain) (x: string) = 
  match e with 
  | [] -> failwith "Binding not found"
  | (ide, value, domain)::r -> if x = ide then 
      (* Reminder that, from the Set library, Set.subset s1 s2 returns true iff s1 is a subset of s2 *)
      if Code.PermSet.subset domain.read_perms d.read_perms then value else failwith ("Unsufficient permissions for " ^ide)
    else lookup_sandboxed r d x
;;
(* Checkup for the permissions of a certain resource in the stack, returning state of the exit + message *)
let rec check_perms (e:'v env) (x: string) (d: Code.domain) (prim_op: Code.primitive) = 
  match e with 
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
  else check_perms r x d prim_op
;;
(* We bind each variable to a value and a domain limiting its access *)
let bind (env:'v env) (x, v, d) = (x,v,d)::env
;;