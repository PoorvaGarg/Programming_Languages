type bigint = sign * int list
    and sign = Neg | NonNeg

exception InvalidException
exception DivisionByZero

let minus b1 = match b1 with
      (s1,[]) -> (NonNeg,[])
    | (Neg,xs) -> (NonNeg,xs)
    | (NonNeg,xs) -> (Neg,xs)

let abs b1 = match b1 with
      (s1,xs) -> (NonNeg,xs)

let rec print_num_nonzero b1 = match b1 with
    (s,[]) -> ""
  | (NonNeg,x::xs) -> String.concat "" [string_of_int x;print_num_nonzero (NonNeg,xs)]
  | (Neg,xs) -> String.concat "" ["-";print_num_nonzero (NonNeg,xs)]

let print_num b1 = match b1 with
    (s,[]) -> "0"
  | (s,xs) -> print_num_nonzero b1

let rec pve_to_list i = if i==0 then []
                        else ((pve_to_list (i/10)) @ [(i mod 10)])

let mk_big i = if i==0 then (NonNeg,[])
               else if i < 0 then (Neg,(pve_to_list (-i)))
               else (NonNeg,(pve_to_list i))

let rec eq b1 b2 = match b1 with
      (s1,[]) -> (match b2 with
                    (s2,[]) -> true
                  | (s2,x::xs) -> false)
    | (Neg,y::ys) -> (match b2 with
                        (s2,[]) -> false
                      | (Neg,x::xs) -> if x==y then (eq (Neg,ys) (Neg,xs)) else false
                      | (NonNeg,x::xs) -> false)
    | (NonNeg,y::ys) -> (match b2 with
                            (s2,[]) -> false
                          | (NonNeg,x::xs) -> if x==y then (eq (NonNeg,ys) (NonNeg,xs)) else false
                          | (Neg,x::xs) -> false)

let rec gt b1 b2 = match b1 with
      (s1,[]) -> (match b2 with
                    (s2,[]) -> false
                  | (Neg,y::ys) -> true
                  | (NonNeg,y::ys) -> false)
    | (Neg,x::xs) -> (match b2 with
                        (NonNeg,ys) -> false
                      | (Neg, []) -> false
                      | (Neg,y::ys) -> if (List.length(x::xs) > List.length(y::ys)) then false
                                       else if (List.length(y::ys) > List.length(x::xs)) then true
                                       else (if (x > y) then false
                                             else if (x < y) then true
                                             else gt (Neg,xs) (Neg,ys)))
    | (NonNeg,x::xs) -> (match b2 with
                            (Neg,ys) -> true
                          | (NonNeg,[]) -> true
                          | (NonNeg,y::ys) -> if (List.length(x::xs) > List.length(y::ys)) then true
                                              else if (List.length(y::ys) > List.length(x::xs)) then false
                                              else (if (x > y) then true
                                                    else if (x < y) then false
                                                    else (gt (NonNeg,xs) (NonNeg,ys))))

let rec lt b1 b2 = match b1 with
      (s1,[]) -> (match b2 with
                    (s2,[]) -> false
                  | (Neg,y::ys) -> false
                  | (NonNeg,y::ys) -> true)
    | (Neg,x::xs) -> (match b2 with
                        (NonNeg,ys) -> true
                      | (Neg,[]) -> true
                      | (Neg,y::ys) -> if List.length(x::xs) > List.length(y::ys) then true
                                       else if List.length(y::ys) > List.length(x::xs) then false
                                       else (if x > y then true
                                             else if x < y then false
                                             else (lt (Neg,xs) (Neg,ys))))
    | (NonNeg,x::xs) -> (match b2 with
                            (Neg,ys) -> false
                          | (NonNeg,[]) -> false
                          | (NonNeg,y::ys) -> if List.length(x::xs) > List.length(y::ys) then false
                                              else if List.length(y::ys) > List.length(x::xs) then true
                                              else (if x > y then false
                                                    else if x < y then true
                                                    else (lt (NonNeg,xs) (NonNeg,ys))))

let geq b1 b2 = let x = (lt b1 b2) in
                        match x with
                           true -> false
                         | false -> true

let leq b1 b2 = let x = gt b1 b2 in
                        match x with
                           true -> false
                         | false -> true

let rec add_lists b1 b2 = let a1 = List.rev(b1) in
                            let a2 = List.rev(b2) in
                                  match a1 with
                                        [] -> b2
                                      | x::xs -> (match a2 with
                                                        [] -> b1
                                                      | y::ys -> (add_lists (List.rev(xs)) (List.rev(ys))) @ [(x+y)])

(*designed assuming that b1>b2*)
let rec sub_lists b1 b2 = let a1 = List.rev(b1) in
                            let a2 = List.rev(b2) in
                                  match a1 with
                                        [] -> b2
                                      | x::xs -> (match a2 with
                                                        [] -> b1
                                                      | y::ys -> (sub_lists (List.rev(xs)) (List.rev(ys))) @ [(x-y)])

(* with lsb first *)
let rec correct_list b = match b with
                                [] -> []
                              | [x] -> if x > 9 then (x mod 10)::(correct_list [(x/10)]) else [x]
                              | x::y::xs -> if x>9 then (x mod 10)::(correct_list ((y+x/10)::xs))
                                            else if x<0 then (correct_list ((x+10)::((y-1)::xs)))
                                            else x::(correct_list (y::xs))

let rec rm_0 b = match b with
                    (s,[]) -> (s,[])
                  | (s,x::xs) -> if x==0 then (rm_0 (s,xs)) else b

let rec rm_0_l b = match b with
                    [] -> []
                  | x::xs -> if x==0 then (rm_0_l xs) else b

let add b1 b2 = match b1 with
      (s,[]) -> b2
    | (NonNeg,xs) -> (match b2 with
                        (s,[]) -> b1
                      | (NonNeg,ys) -> (rm_0 (NonNeg, List.rev(correct_list (List.rev(add_lists xs ys)))))
                      | (Neg,ys) -> (match (geq (NonNeg,xs) (NonNeg,ys)) with
                                          true -> (rm_0 (NonNeg, List.rev(correct_list (List.rev(sub_lists xs ys)))))
                                        | false -> (rm_0 (Neg, List.rev(correct_list (List.rev(sub_lists ys xs)))))))
    | (Neg,xs) -> (match b2 with
                      | (s,[]) -> b1
                      | (NonNeg,ys) -> (match (gt (NonNeg,xs) (NonNeg,ys)) with
                                          true -> (rm_0 (Neg, List.rev(correct_list (List.rev(sub_lists xs ys)))))
                                        | false -> (rm_0 (NonNeg, List.rev(correct_list (List.rev(sub_lists ys xs))))))
                      | (Neg,ys) -> (rm_0 (Neg,List.rev(correct_list (List.rev(add_lists xs ys))))))

let sub b1 b2 = add b1 (minus b2)

(* with msb first *)
let rec mult_lists b1 b2 = match b2 with
          [] -> []
        | [x] -> (match b1 with
                    [] -> []
                  | y::ys -> (x*y)::(mult_lists ys b2))
        | x::xs -> (match b1 with
                    [] -> []
                  | [y] -> mult_lists b2 b1
                  | y::ys -> List.rev(add_lists (List.rev(mult_lists b2 [y])) ((List.rev(0::(mult_lists b2 ys))))))

let mult b1 b2 = match b1 with
        (NonNeg,xs) -> (match b2 with
                      (NonNeg,ys) -> (rm_0 (NonNeg, List.rev(correct_list (List.rev(mult_lists xs ys)))))
                    | (Neg,ys) -> (rm_0 (Neg, List.rev(correct_list (List.rev(mult_lists xs ys))))))
      | (Neg,xs) -> (match b2 with
                    (NonNeg,ys) -> (rm_0 (Neg, List.rev(correct_list (List.rev(mult_lists xs ys)))))
                  | (Neg,ys) -> (rm_0 (NonNeg, List.rev(correct_list (List.rev(mult_lists xs ys))))))

let rec div_sub b1 b2 ans = match b2 with
            [] -> raise DivisionByZero
          | xs -> (match (geq (NonNeg,(rm_0_l b1)) (NonNeg,b2)) with
                                true -> (div_sub ((List.rev(correct_list (List.rev((sub_lists b1 b2)))))) b2
                                (List.rev(correct_list (List.rev(add_lists [1] ans)))))
                              | false -> ans)

let rec divide_q b1 b2 q r = match b2 with
            [] -> raise DivisionByZero
            | xs -> (match b1 with
                      [] -> q
                    | x::xs -> let new_q = (div_sub (r @ [x]) b2 []) in
                                    let q_mid = (if new_q = [] then (q @ [0]) else (q @ new_q)) in
                                        let r_mid = rm_0_l (sub_lists (r @ [x]) (List.rev(correct_list (List.rev(mult_lists new_q b2))))) in
                                                (divide_q xs b2 q_mid r_mid));;

let rec divide_r b1 b2 q r = match b2 with
            [] -> raise DivisionByZero
          | xs -> (match b1 with
                      [] -> r
                    | x::xs -> let new_q = (div_sub (r @ [x]) b2 []) in
                                    let q_mid = (if new_q = [] then (q @ [0]) else (q @ new_q)) in
                                        let r_mid = rm_0_l (sub_lists (r @ [x]) (List.rev(correct_list (List.rev(mult_lists new_q b2))))) in
                                              (divide_r xs b2 q_mid r_mid))

let div b1 b2 = match b2 with
      (s,[]) -> raise DivisionByZero
    | (NonNeg,ys) -> (match b1 with
                          (s,[]) -> (NonNeg,[])
                        | (NonNeg,xs) -> (NonNeg,(rm_0_l (List.rev(correct_list (List.rev(divide_q xs ys [] []))))))
                        | (Neg,xs) -> (Neg,(rm_0_l (List.rev(correct_list (List.rev(divide_q xs ys [] [])))))))
    | (Neg,ys) -> (match b1 with
                          (s,[]) -> (NonNeg,[])
                        | (NonNeg,xs) -> (Neg,(rm_0_l (List.rev(correct_list (List.rev(divide_q xs ys [] []))))))
                        | (Neg,xs) -> (NonNeg,(rm_0_l (List.rev(correct_list (List.rev(divide_q xs ys [] [])))))))

let rem b1 b2 = match b2 with
      (s,[]) -> raise DivisionByZero
    | (NonNeg,ys) -> (match b1 with
                          (s,[]) -> (NonNeg,[])
                        | (NonNeg,xs) -> (NonNeg,(rm_0_l (List.rev(correct_list (List.rev(divide_r xs ys [] []))))))
                        | (Neg,xs) -> (Neg,(rm_0_l (List.rev(correct_list (List.rev(divide_r xs ys [] [])))))))
    | (Neg,ys) -> (match b1 with
                          (s,[]) -> (NonNeg,[])
                        | (NonNeg,xs) -> (NonNeg,(rm_0_l (List.rev(correct_list (List.rev(divide_r xs ys [] []))))))
                        | (Neg,xs) -> (Neg,(rm_0_l (List.rev(correct_list (List.rev(divide_r xs ys [] [])))))))
