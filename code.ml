open Env

(* Set of basic operations concerning files – reading, writing, deleting and sending. *)
type file_ops = Read | Write | Delete | Send
(* Permissions for interacting with files, represented as the right to perform some actions over some files *)
(* type file_perm = (string * file_ops) *)
;;
(* A mobile code is restricted to a domain, which specifies for the code (tagged by a specific url in Java-style)
 a set of file permissions and resource permissions *)
type domain = {
  url : string;
  (* f_perms : file_perm list; *)
  r_perms : Env.PermSet.t;
}
;;

let grantResPerm (dom : domain) (perm : Env.PermSet.elt) : domain = {
  url = dom.url;
  (* f_perms = dom.f_perms; *)
  r_perms = Env.PermSet.add perm dom.r_perms
} 

(* let grantFilePerm (dom : domain) (perm : file_perm) : domain = {
  url = dom.url;
  f_perms = perm::dom.f_perms;
  r_perms = dom.r_perms
}  *)

