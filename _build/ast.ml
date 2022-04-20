type ide = string 

(* Describing the basic operations between values *)
type op = Sum | Minus | Times | Divide | Equal | Less | Greater

(*
The language has been extended with booleans, Functions and functions calls and read, write, open operations. These three represent a generic 
interaction with a system resource. THey could also be represented as particular functions but i preferred to explicitly represent them 
to have more clear examples.
 *)
type exp = Eint of int
         | Den of ide 
         | Op of op*exp*exp
         | If of exp*exp*exp
         | Let of ide*exp*exp
         | LetEm of Policy.policy * ide * exp * exp
         | Fun of ide * exp
         | Call of exp*exp
         | Read of ide
         | Write of ide
         | Open of ide
;;


(*
The expression can be evaluated by the interpreter to an integer, a boolean, or a closure which is the runtime value of functions expressions.
 *)
type  value = Int of int 
            | Bool of bool
            | Closure of  (ide*exp*value Env.t)
(* a closure is made of the function name, the function argument, the body of the function and the symbol table with the values captured. We assume for simplicity to have only one parameter per function,
   which is similar to function abstractions in the lambda calculus where we obtain multi parameter functions by chaining single parameter ones.*)
