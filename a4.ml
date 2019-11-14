open A1
exception Not_implemented
exception Invalid_exp

(* Assumptions made : if one level deep Function abstraction is encountered then formal parameter
should be present in gamma table
Function to infer types has been made but not used due to the above assumption
To check for a particular type it should be well typed first in case of function call and
definitions, it should *)

(* Function to apply f on l and get all lists append *)
let rec apply_append f l = match l with [] -> []
                                    | x::xs -> (f x) @ (apply_append f xs)

(* Function to infer the type of a variable from its occurences in an expression *)
let rec infer s e = match e with
      Var(x) -> [Tunit]
    | N(x) -> [Tunit]
    | B(x) -> [Tunit]
    | Abs(e1) | Negative (e1) -> if e1 = Var(s) then [Tint] else (infer s e1)
    | Not(e1) -> if e1 = Var(s) then [Tbool] else (infer s e1)
    | Add(e1,e2) | Sub(e1,e2) | Mult(e1,e2) | Div(e1,e2) | Rem(e1,e2)
    | Equals(e1,e2) | GreaterT(e1,e2) | LessT(e1,e2) | GreaterTE(e1,e2) | LessTE(e1,e2) ->
                          (if e1 = Var(s) then
                              (if e2 = Var(s) then [Tint] else (Tint::(infer s e2)))
                          else (if e2 = Var(s) then (Tint::(infer s e2)) else ((infer s e1)) @ (infer s e2)))
    | Conjunction(e1,e2) | Disjunction(e1,e2) ->
                          (if e1 = Var(s) then
                              (if e2 = Var(s) then [Tbool] else (Tbool::(infer s e2)))
                          else (if e2 = Var(s) then (Tbool::(infer s e2)) else ((infer s e1) @ (infer s e2))))
    | InParen(e1) -> (infer s e1)
    | IfThenElse(e1,e2,e3) -> (if e1 = Var(s) then Tbool::((infer s e2) @ (infer s e3))
                               else ((infer s e1) @ (infer s e2) @ (infer s e3)))
    | Tuple(i,l) -> apply_append (infer s) l
    | Project((i,n),e1) -> (infer s e1)
    | Let(d,e1) -> raise Not_implemented
    | FunctionCall(e1,e2) -> raise Not_implemented
    | FunctionAbstraction(str,e1) -> raise Not_implemented

(* a boolean function to pass as an argument in infer_list *)
let eq a b = (a=b)

(* To check uniformity in infer_list *)
let infer_list s e = let l = (infer s e) in let boo = (List.for_all (eq (List.hd(l))) l) in
                        (match boo with true -> List.hd(l)
                                      | false -> raise Invalid_exp)

(* To check existence of a key in a list without error or different data type *)
let rec exist_key s l = match l with
    [] -> false
  | x::xs -> (match x with
                  (y,t) -> (if y = s then true else (exist_key s xs)))

(* To remove extra occurences *)
let rec sequenceL g l = match g with
            [] -> l
          | x::xs -> (match x with
                      (s,t) -> if (exist_key s l) then
                                    (let pair = (List.assoc s l) in
                                          (if pair = t then (sequenceL xs l)
                                           else (sequenceL xs l)))
                               else (sequenceL xs (x::l)))

(* To remove duplicate occurences *)
let rec parallelL g l = match g with
            [] -> l
          | x::xs -> (match x with
                      (s,t) -> (if (exist_key s l) then
                                    (let pair = (List.assoc s l) in
                                          (if pair = t then (parallelL xs l)
                                           else raise Invalid_exp))
                               else (parallelL xs (x::l))))

(* To get type of an expression *)
let rec give_type g e =

    (* To yield proper table for definitions *)
    let rec yield_fair g d  = match d with
          Simple(s,e) -> [(s,(give_type g e))]
        | Sequence(l) -> (match l with [xs;ys] -> sequenceL (yield_fair ((yield_fair g xs) @ g) ys) []
                                      | _ -> raise Invalid_exp)
        | Parallel(l) -> (match l with [xs;ys] -> parallelL ((yield_fair g xs) @ (yield_fair g ys)) []
                                      | _ -> raise Invalid_exp)
        | Local(d1,d2) -> yield_fair ((yield_fair g d1) @ g) d2
    in

    (match e with
        Var(s) -> List.assoc s g
      | N(n) -> Tint
      | B(b) -> Tbool
      | Abs(e1) -> if ((give_type g e1) = Tint) then Tint else raise Invalid_exp
      | Negative(e1) -> if ((give_type g e1) = Tint) then Tint else raise Invalid_exp
      | Not(e1) -> if ((give_type g e1) = Tbool) then Tbool else raise Invalid_exp
      | Add(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tint
                                                                                else raise Invalid_exp
      | Sub(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tint
                                                                                else raise Invalid_exp
      | Mult(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tint
                                                                                else raise Invalid_exp
      | Div(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tint
                                                                                else raise Invalid_exp
      | Rem(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tint
                                                                                else raise Invalid_exp
      | Conjunction(e1,e2) -> if ((give_type g e1) = Tbool) && ((give_type g e2) = Tbool) then Tbool
                                                                                else raise Invalid_exp
      | Disjunction(e1,e2) -> if ((give_type g e1) = Tbool) && ((give_type g e2) = Tbool) then Tbool
                                                                                else raise Invalid_exp
      | Equals(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tbool
                                                                                else raise Invalid_exp
      | GreaterTE(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tbool
                                                                                else raise Invalid_exp
      | LessTE(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tbool
                                                                                else raise Invalid_exp
      | GreaterT(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tbool
                                                                                else raise Invalid_exp
      | LessT(e1,e2) -> if ((give_type g e1) = Tint) && ((give_type g e2) = Tint) then Tbool
                                                                                else raise Invalid_exp
      | InParen(e1) -> (give_type g e1)
      | IfThenElse(e1,e2,e3) -> if ((give_type g e1) = Tbool)
                                                  && ((give_type g e2) = (give_type g e3)) then (give_type g e2)
                                                  else raise Invalid_exp
      | Tuple(i,l) -> if List.length(l) = i then Ttuple(List.map (give_type g) l) else raise Invalid_exp
      | Project((i,n),e1) -> (match (give_type g e1) with
                                    Ttuple(l) -> (if List.length(l) = n then (List.nth l (i-1))
                                                 else raise Invalid_exp)
                                  | _ -> raise Invalid_exp)
      | Let(d,e1) -> give_type ((yield_fair g d) @ g) e1
      | FunctionAbstraction(s,e1) -> (let t1 = (List.assoc s g) in
                                          let t2 = (give_type ((s,t1)::g) e1) in Tfunc(t1,t2))
      | FunctionCall(e1,e2) -> (match (give_type g e1) with
                                            Tfunc(t1,t2) -> if t1 = (give_type g e2) then t2 else raise Invalid_exp
                                          | _ -> raise Invalid_exp))
(* To give a rough incremental table *)
let rec yield_what g d = match d with
              Simple(s,e) -> [(s,(give_type g e))]
            | Sequence(l) -> (match l with
                                [] -> []
                              | x::xs -> (yield_what ((yield_what g x) @ g) (Sequence(xs))) @ (yield_what g x))
            | Parallel(l) ->  (match l with
                                [] -> []
                              | x::xs -> (yield_what ((yield_what g x) @ g) (Parallel(xs))) @ (yield_what g x))
            | Local(d1,d2) -> (yield_what ((yield_what g d1) @ g) d2)

(* To yield the incremental table *)
let rec yield_fair g d  = match d with
      Simple(s,e) -> [(s,(give_type g e))]
    | Sequence(l) -> (match l with [xs;ys] -> sequenceL ((yield_fair ((yield_fair g xs) @ g) ys) @ (yield_fair g xs)) []
                                  | _ -> raise Invalid_exp)
    | Parallel(l) -> (match l with [xs;ys] -> parallelL ((yield_fair g xs) @ (yield_fair g ys)) []
                                  | _ -> raise Invalid_exp)
    | Local(d1,d2) -> yield_fair ((yield_fair g d1) @ g) d2


(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t = match e with
          Var(s) -> List.mem (s,t) g
        | N(n) -> (match t with Tint -> true
                            | _ -> false)
        | B(b) -> (match t with Tbool -> true
                            | _ -> false)
        | Abs(exp) -> (hastype g exp Tint) && (t=Tint)
        | Negative(exp) -> (hastype g exp Tint) && (t = Tint)
        | Not(exp) -> (hastype g exp Tbool) && (t = Tbool)
        | Add(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
        | Sub(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
        | Mult(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
        | Div(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
        | Rem(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tint)
        | Conjunction(e1,e2) -> (hastype g e1 Tbool) && (hastype g e2 Tbool) && (t = Tbool)
        | Disjunction(e1,e2) -> (hastype g e1 Tbool) && (hastype g e2 Tbool) && (t = Tbool)
        | Equals(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
        | GreaterTE(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
        | LessTE(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
        | GreaterT(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
        | LessT(e1,e2) -> (hastype g e1 Tint) && (hastype g e2 Tint) && (t = Tbool)
        | InParen(exp) -> (hastype g exp t)
        | IfThenElse(e1,e2,e3) -> (hastype g e1 Tbool) && (hastype g e2 t) && (hastype g e3 t)
        | Tuple(i,l) -> (let rec list_hastype g el tl = (match el with
                                  [] -> (match tl with
                                            [] -> true
                                          | _ -> false)
                                | x::xs -> (match tl with
                                                [] -> false
                                              | y::ys -> (hastype g x y) && (list_hastype g xs ys))) in

                        (if List.length(l)=i then
                                (match t with Ttuple(et) ->  (list_hastype g l et)
                                           | _ -> false)
                        else raise Invalid_exp))
        | Project((i,n),e) -> (match (give_type g e) with
                                  Ttuple(l) -> (if (List.length(l)=n) then (t = (List.nth l (i-1)))
                                                  else raise Not_implemented)
                                | _ -> raise Not_implemented)
        | Let(d,e) -> hastype ((yield_fair g d) @ g) e t
        | FunctionAbstraction(s,e) -> (match t with
                                          Tfunc(t1,t2) -> hastype ((s,t1)::g) e t2
                                        | _ -> false)
        | FunctionCall(e1,e2) -> let t2 = (give_type g e2) in (hastype g e1 (Tfunc(t2,t)))





(* Not considering duplicates in g_dash *)
(* if a exists in l or not *)
let user_mem l a = List.mem a l

(* check presence of elements of l1 in l2 *)
let rec check l1 l2 = List.for_all (user_mem l2) l1

(* if two lists are equal as set or not *)
let equality l1 l2 = (List.length(l1) = List.length(l2)) && (check l1 l2) && (check l2 l1)



(* Actual required yields function *)
let yields g d g_dash = equality g_dash (yield_fair g d)
