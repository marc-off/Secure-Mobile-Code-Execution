open Printf
(* Helper function which prints out the contents of a local environment, useful for debugging purposes *)
let format_env (my_e : Ast.value Env.env) = 
  let scan_row row =  
    match row with 
    | (id, (v: Ast.value), (d: Code.domain)) -> 
    let print_ide = "Identifier: "^id 
    and print_perms = List.fold_left (fun l1 l2 -> l1^l2) String.empty in
    let print_rperms = (Code.PermSet.elements d.read_perms) |> print_perms
    and print_wperms = (Code.PermSet.elements d.write_perms) |> print_perms
    and print_operms = (Code.PermSet.elements d.open_perms) |> print_perms
    and print_sendperms = (Code.PermSet.elements d.send_perms) |> print_perms in
      match v with
      | Int(i) -> Printf.printf "%s - Value: %i - R_perms: %s - W_perms: %s - O_perms: %s - S_perms: %s\n" print_ide i print_rperms print_wperms print_operms print_sendperms
      | Bool(b) -> Printf.printf "%s - Value: %B - R_perms: %s - W_perms: %s - O_perms: %s - S_perms: %s\n" print_ide b print_rperms print_wperms print_operms print_sendperms
      | Closure(id', _, _) -> Printf.printf "%s - Closure: %s - R_perms: %s - W_perms: %s - O_perms: %s - S_perms: %s\n" print_ide id' print_rperms print_wperms print_operms print_sendperms
  in List.iter scan_row my_e.state
;;
(* Function for formatting the battery test, ensuring that the real output matches the expected one *)
let format_test tpair = match tpair with
	| (a, b) -> let () = printf "EXPECTED RES: %s - " a in match b with
		| None -> (
			if a="ABORT" then printf "OK TEST: execution aborted as expected!\n" 
				else printf "ERROR test (computation labelled as 'OK' returned no value)\n"
		)
		| Some v -> (
			if a="OK" then let ok_string = "OK TEST: the returned value is" in match v with
				| Ast.Int(n) -> printf "%s: %i\n" ok_string n
				| Ast.Bool(b) -> printf "%s: %B\n" ok_string b
				| Ast.Closure(id, _exp, _env) -> printf "%s the closure: %s\n" ok_string id
			else printf "ERROR test (computation labelled as 'ABORT' returned a value instead)\n"
		)
;;