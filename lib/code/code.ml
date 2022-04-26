(* These are the primitives, abstracting from operations with resources, for which we define a set of permissions *)
type primitive = Read | Write | Send
;;
(* Every primitive operations with a resource is protected by a set of required permissions – 
    for simplicity, perms will be rappresented by a tag *)
module PermSet = Set.Make(String)
;;
(* Each mobile code is assigned to a domain, enabling for the code (tagged by a specific url in Java-style)
 a set of permissions for primitive operations *)
type domain = {
  url : string;
  read_perms : PermSet.t;
  write_perms : PermSet.t;
  send_perms : PermSet.t;
}
;;
let emptyDomain : domain = {
  url="";
  read_perms=PermSet.empty;
  write_perms=PermSet.empty;
  send_perms=PermSet.empty;
}
;;

