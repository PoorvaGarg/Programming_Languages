(* Dummy implementation of A1 *)
open A0
exception Not_implemented

(* to get ith element of list *)
let rec proj i l = if i > List.length(l) then raise Not_implemented
                   else (if i = 1 then List.hd(l)
                   else (match l with x::xs -> (proj (i-1) xs)
                                    | _ -> raise Not_implemented))
let rec take_n n l ans = if n>0 then
                            (match l with
                                  x::xs -> take_n (n-1) xs (x::ans)
                                | _ -> raise Not_implemented)
                        else ans


let rec del_n n l ans = if n>0 then
                            (match l with
                                  x::xs -> del_n (n-1) xs (x::ans)
                                | _ -> raise Not_implemented)
                        else l

let rec map fn l = match l with
    [] -> []
    | x::xs -> (fn x) @ (map fn xs)

let rec maps fn l rho = match l with
    [] -> []
    | x::xs -> (fn x rho)::(maps fn xs rho)


(* abstract syntax *)
type  exptree =
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | Let of definition * exptree
  | FunctionAbstraction of string * exptree
  | FunctionCall of exptree * exptree
(* definition *)
and definition =
    Simple of string * exptree
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int | LET | FABS | FCALL
  | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

let rec eval ex rho = raise Not_implemented


let stackmc stk binding pgm = raise Not_implemented

let rec compile def = raise Not_implemented (* match def with
    Simple(s,e) -> [VAR(s)] @ (compile e) @ [SIMPLEDEF]
  | Sequence(l) -> (match l with
                        [] -> [SEQCOMPOSE]
                      | x::xs -> (compile x) @ (compile Sequence(xs)))
  | Parallel(l) -> (match l with
                        [] -> [PARCOMPOSE]
                      | x::xs -> (compile x) @ (compile Parallel(xs)))
  | Local(d1,d2) -> (compile d1) @ (compile d2) @ [LOCALDEF] *)

let rec compile ex = raise Not_implemented (* match ex with
        Var(s) -> [VAR(s)]
      | N(i) -> [NCONST(mk_big i)]
      | B(b) -> [BCONST(b)]
      | Abs(t) -> (compile t) @ [ABS]
      | Negative(t) -> (compile t) @ [UNARYMINUS]
      | Not(t) -> (compile t) @ [NOT]
      | Add(t1, t2) -> (compile t1) @ (compile t2) @ [PLUS]
      | Sub(t1, t2) -> (compile t1) @ (compile t2) @ [MINUS]
      | Mult(t1, t2) -> (compile t1) @ (compile t2) @ [MULT]
      | Div(t1, t2) -> (compile t1) @ (compile t2) @ [DIV]
      | Rem(t1, t2) -> (compile t1) @ (compile t2) @ [REM]
      | Conjunction(t1, t2) -> (compile t1) @ (compile t2) @ [CONJ]
      | Disjunction(t1, t2) -> (compile t1) @ (compile t2) @ [DISJ]
      | Equals(t1,t2) -> (compile t1) @ (compile t2) @ [EQS]
      | GreaterTE(t1,t2) -> (compile t1) @ (compile t2) @ [GTE]
      | LessTE(t1,t2) -> (compile t1) @ (compile t2) @ [LTE]
      | GreaterT(t1,t2) -> (compile t1) @ (compile t2) @ [GT]
      | LessT(t1,t2) -> (compile t1) @ (compile t2) @ [LT]
      | InParen(t) -> (compile t) @ [PAREN]
      | IfThenElse(t1,t2,t3) -> (compile t1) @ (compile t2) @ (compile t3) @ [IFTE]
      | Tuple(i,l) -> (map compile l) @ [TUPLE(i)]
      | Project((i1,i2),t) -> (compile t) @ [PROJ(i1,i2)]
      | Let(d,e) -> (compile d) @ (compile e)
      | FunctionAbstraction(s,e) -> [VAR(s)] @ (compile e) @ [FABS]
      | FunctionCall(e1,e2) -> (compile e1) @ (compile e2) @ [FCALL] *)
