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
(* 
let grantResPerm (dom : domain) (perm : PermSet.elt) : domain = {
  url = dom.url;
  (* f_perms = dom.f_perms; *)
  r_perms = PermSet.add perm dom.r_perms
} 
;; *)
(* let grantFilePerm (dom : domain) (perm : file_perm) : domain = {
  url = dom.url;
  f_perms = perm::dom.f_perms;
  r_perms = dom.r_perms
}  *)

