
(*
  We model security policies as automatas.
*)
type state = int
;;
(*
  We assume to have four relevant events to control within our polices: Read operations, Write operations, Open operations and Send operations.
  In this implementation these are tied to the homonymous ast nodes but it is not necessary (for example they could be tied to library functions with that functionality.)
  In a more complex implementation we would let events to be user defined, but for simplicity i chose to fix them)
*)
type event =
  | Read
  | Write
  | Open
  | Send
;;
(* 
  The automata associated to a policy. The transition relation is provided by the user.  In a real language, we would
  have a way to transform a higher level syntax maybe internal to the language itself to an automata. For simplicity i assumed
  that this step is already done
*)
type policy = {
  states: state list;
  init_state: state ;
  delta: state -> event -> state option; (* the automata is assumed to be deterministic for simplicity *)
  accept_states: state list
}
;;
(* Applies the transition function with respect to the semantics of automata *)
let transition (dfa: policy)  (state: state) event =
  event |> dfa.delta state
;;
(* Checks that a trace of events satisfy a certain policy. *)
let check_accepts (events: event list) (dfa:policy) =

  (* Essentially we keep applying the transition function until we have a dead transtion (None) or we consumed the entire trace.
  When the trace is consumed we check if the final state is an accepting one*)
  let aux_check state event =
    if Option.is_none state
    then None
    else
      event |> transition dfa (Option.get state)
  in
  List.fold_left aux_check (Some dfa.init_state) events
  |> (fun e -> if Option.is_some e then  dfa.accept_states |> List.mem (Option.get e) else false)
;;
(* checks that the given trace of events satisies a list of security policies. Returns true only if all policies are satisfied*)
let check_policies  events (policies: policy list)=
  policies
  |> List.map (check_accepts events)
  |> List.fold_left (&&) true
;;
(* For each evaluation step, the current expression is saved to form a final trace to analyse *)
let get_trace (exp: Ast.expr) =
  match exp with
  | Ast.Read _ -> [Read]
  | Ast.Write _ -> [Write]
  | Ast.Open _ -> [Open]
  | Ast.Send _ -> [Send]
  | _ -> []
;;