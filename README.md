# Homework Assignment 1 – Language-Based Technology for Security - A.A. 2021/2022

## In this homework, you will design a simple functional language having primitive abstractions for securing the execution of mobile code. Mobile code is a programming approach where programs are considered as data, and therefore are accessible, transmitted, and evaluated. In this assignment, we refer to mobile code as software that travels on a heterogeneous network, crossing protection domains, and automatically executed upon arrival at the destination. Protection domains can be as big as corporate clouds and as small as smartphones

### **General Description**

***

#### In this project, we created a simple functional programming language and the related interpreter. There is only one interpreter 'eval', but it takes one more parameter, called 'sandbox'. This one, allow it to distinguish two modes of operation:  
* Basic interpreter
* Sandbox
#### There is a global environment, in which are saved the triples (identifier, value, domain). The main difference between the basic interpreter and the sandbox is that for the first one, every modify and/or access operation related to the environment is allowed, instead, for the sandbox (when the flag is set) the operations related to the environment are restricted. In this last case, the bind operations have effects only in the sandbox scope and at the end of the mobile code evaluation, the global environment will not be modified, compared to the environment passed as an argument at the beginning of the evaluation. The sandbox mode is executed with a domain thath could be changed through a runtime function. Moreover, every attempt to access the global environment will be subjected to checks that will verify that the domain of permissions with which the sandbox is executed is a superset of the domain associated with the element of the environment to which it wants to access.


### **How to Run**

***

#### To run the following project, it's necessary to have [**dune**](https://dune.build/) installed, a build system for OCaml projects. Other components necessary for running the projects are installed from the tutorial in the [**OCaml homepage**](https://ocaml.org/learn/tutorials/up_and_running.html). Now we assume you're inside the homework directory with the following layout:

    homework/
    ├─ bin/
    │  ├─ dune
    │  ├─ main.ml
    ├─ lib/
    │  ├─ ast/
    |  |  ├─ ast.ml  
    │  ├─ code/
    │  │  ├─ code.ml
    │  ├─ debug/
    │  │  ├─ debug.ml
    │  ├─ env/
    │  │  ├─ env.ml
    │  ├─ execute/
    │  │  ├─ execute.ml
    │  ├─ interpreter/
    │  │  ├─ interpreter.ml
    │  ├─ policy/
    │  │  ├─ policy.ml
    │  ├─ dune
    │  ├─ lib.ml
    ├─ test/
    │  ├─ dune
    │  ├─ homework.ml
    ├─ Consegna.pdf
    ├─ homework.opam
    ├─ README.md

#### The steps to run the project are now the following:

1. `dune build ./bin/main.exe` 

2. `dune exec ./bin/main.exe` 

### **Example of Test**

***

#### The expressions evaluated are inspired by the homework, and are the following: 

> 1. ((fun x = x+1)6)
>
> 2. let equal_5 x = (x=5);; execute(let result = equal_5(5));;
>
> 3. let mypin = 12345;; execute(let result = mypin in send(result))

#### The output we expect to have is the following:

#### `Send OK`
#### `EXPECTED RES: OK - OK TEST: the returned value is: 6`
#### `EXPECTED RES: OK - OK TEST: the returned value is: true`
#### `EXPECTED RES: OK - OK TEST: the returned value is: true`

## **Library**

### **Debug**

***

#### This module contains some functionalities that help the programmer with the *debugging process* of the project e.g. printing the content of an environment, formatting the results of our testing.

### **Interpreter**

***

#### Our interpreter mantains overall the usual semantics: it takes in input an *expression* in the form of an **abstract syntax tree**, and returns a *value*, since we're modelling a simple functional programming language. The changes we've made accomodate the restriction of our evaluation with respect to the *definition of policies and permissions of a domain*. 

### **Ast**

***

#### Our Abstract Syntax Tree models a *simple functional programming language*, highlighting the idea that each expression is evaluated through an interpreter, and returns a simple value. 

### **Code**

***

#### Every piece code is restricted to a specific **domain**, which identifies the origin of that code and the *set of permissions* assigned to that code. For simplicity, the permissions inside the domain are used for protecting the variables inside that code from external accesses, and for that same code to access external variables defined in other codes

> #### Example
> ####
> #### Assuming we have a set of permissions S1 = {A, B}, which restricts the access to a variable 'x' defined within the domain D1 of code C1; Assuming that another code C2, equipped with another set of permissions S2 = {A}, wants to access to the variable 'x' of the code C1. According to the logic of sets, the check of the permissions is successful if S1 ⊆ S2 (for instance, in this case {A, B} ⊈ {A}, meaning that the check of permission fails)
 
### **Env**

***

#### The environment provides some functionalities to manage the binding of a variable, to check the permissions for specific operations and to lookup for the value of an occurrence. We tweaked the implementation of the environment, introducing a **local** definition of the same where a binding can now *change the state of the environment* (even after evaluating an expression containing that binding). The lookup has also been modified, conformally to the definition of permissions

### **Execute**

***

#### We defined two data structure, respectively for the list of policy to pass to the execute function and the domain associated to this one. This module provides three runtime functions to modify the data structures previously introduced. The execute function calls the interpreter eval with:

* the sandbox flag *set* to **true**
* the two data structures - **policies** and **domain**
* the **local environment** as the env parameter

#### So, in this case, differently from the one in which the sandbox flag is not set, every lookup operation required by the mobile code (passed as expression to the execute function), will be subjected to permissions checks, mandatory for accessing to the elements of the local environment, and the bind operations will not modify this one

### **Policy**

***

#### In the policy module, we model security policies as automatas. There are four relevant events for security to check with the security policies: read, write, open and send operations. In this module are defined the transition function, check_accepts that check a trace of events satisfy a certain policy: the transition function is applied until there is a dead transition or the entire trace is consumed. And, in the end, check_policies that checks that the given trace of events satisfies a list of security policies. If and only if all policies are satisfied, this last one returns true