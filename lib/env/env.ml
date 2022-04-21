(* Every resource is protected by a set of required permissions – for simplicity, perms will be rappresented by a tag *)
module PermSet = Set.Make(String)
;;
(* An environment is characterized by an identifier, a generic type 'v and a set of perms required for its access *)
type 'v env = (string * 'v * PermSet.t) list 
(* Initializing the environment, which is set to empty *)
let emptyenv = [];;
(* Semantic of lookup abstracting from permissions provided, as if ran with root perms *)
let rec lookup_root (e: 'v env) (x: string) =
  match e with 
    | [] -> failwith "Binding not found!"
    | (ide, value, _perms)::r -> if x = ide then value else lookup_root r x
;;
(* Semantic of lookup is restricted to the set of perms provided *)
let rec lookup_sandboxed (e:'v env) (x: string) (p: PermSet.t)  = 
  match e with 
  | [] -> failwith "Not found"
  | (ide, value, perms)::r -> if x = ide then 
      (* Reminder that, from the Set library, Set.subset s1 s2 returns true iff s1 is a subset of s2 *)
      if PermSet.subset perms p then value else failwith ("Unsufficient permissions for " ^ide)
    else lookup_sandboxed r x p;;
(* We bind each variable to a value and a set of permissions required for its access *)
let bind (env:'v env) (x: string) v (p: PermSet.t) = (x,v,p)::env;;