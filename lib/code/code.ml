(* These are the primitives, abstracting from operations with resources, for which we define a set of permissions *)
type primitive = Read | Write | Open | Send
;;
(* Every primitive operations with a resource is protected by a set of required permissions – 
    for simplicity, perms will be rappresented by a set of tags e.g. capital letters in our case *)
module PermSet = Set.Make(String)
;;
(* Each mobile code is represented by a domain (Java style-like), enabling for the code a set of permissions for primitive operations.
	At the same time, for simplicity, the permissions specified in the domain are bound to the resources defined in the same mobile code. *)
type domain = {
  codebase : string;
  read_perms : PermSet.t;
  write_perms : PermSet.t;
  open_perms : PermSet.t;
  send_perms : PermSet.t;
}
;;
(* Initializing an empty domain, with no permissions and empty codebase *)
let emptyDomain : domain = {
  codebase="";
  read_perms=PermSet.empty;
  write_perms=PermSet.empty;
  open_perms=PermSet.empty;
  send_perms=PermSet.empty;
}
;;

(* –––––––––––– Predefined sets of permissions, for the sake of our testing –––––––––––– *)

(* permset_A = {A} *)
let permset_A = 
	 PermSet.empty
	|>  PermSet.add "A"
;;
(* permset_B = {B} *)
let permset_B = 
	 PermSet.empty
	|>  PermSet.add "B"
;;
(* permset_C = {C} *)
let permset_C = 
	 PermSet.empty
	|>  PermSet.add "C"
;;
(* permset_AB = {A, B} *)
let permset_AB = 
	 PermSet.empty
	|> List.fold_right  PermSet.add ["A"; "B"]
;;
(* permset_BC = {B, C} *)
let permset_BC = 
	 PermSet.empty
	|> List.fold_right  PermSet.add ["B"; "C"]
;;
(* permset_AC = {A, C} *)
let permset_AC = 
	 PermSet.empty
	|> List.fold_right  PermSet.add ["A"; "C"]
;;
(* permset_ABC = {A, B, C} *)
let permset_ABC = 
	 PermSet.empty
	|> List.fold_right  PermSet.add ["A"; "B"; "C"]
;;
(* We define a set of domains for each mobile code, which represent the mobile code origin and especially the permissions
	 associated to them in the sandbox context. *)
let (domains:  domain list) = [
	{codebase = "https://www.ryanair.com/";
		read_perms = permset_A; write_perms = permset_C; open_perms = permset_C; send_perms = permset_AC};
	{codebase = "https://www.google.it/";
		read_perms = permset_B; write_perms = permset_A; open_perms = permset_A; send_perms = permset_AB};
	{codebase = "https://www.amazon.it/";
		read_perms = permset_C; write_perms = permset_AC; open_perms = permset_AC; send_perms = permset_BC};
	{codebase = "https://www.apple.com/it/";
		read_perms = permset_ABC; write_perms = permset_C; open_perms = permset_C; send_perms = permset_A};
	{codebase = "https://www.microsoft.com/";
		read_perms = permset_BC; write_perms = permset_A; open_perms = permset_A; send_perms = permset_A};
];;
