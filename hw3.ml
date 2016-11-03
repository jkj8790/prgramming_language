open Printf

(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
(*
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
          match exp with
          | Const n -> Const 0
          | Var v -> if v = var then Const 1 else Var v
          | Power (v, n) -> if v = var then Times [Const n; Power (v, n-1)] else Const 0
          | Times li -> 
            begin match li with
              | [] -> Const 0
              | hd::tl -> Const 1
            end
          | Sum li -> 
            begin match li with 
              | [] -> Const 0
              | hd::tl -> Sum [diff (hd, var); diff (Sum tl, var)]
            end;;

end

open Problem1
let example = Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1];;
diff (example, "x");;
let yaho = Sum [Power ("y", 3); Power ("x", 2)];;
diff (yaho, "x");;
*)



(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec weightSum : mobile -> int
  = fun mob ->
     match mob with
     | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
     | (CompoundBranch (l1, sub_mob1), SimpleBranch (l2, w2)) -> weightSum(sub_mob1) + w2
     | (SimpleBranch (l1, w1), CompoundBranch (l2, sub_mob2)) -> w1 + weightSum(sub_mob2)
     | (CompoundBranch (l1, sub_mob1), CompoundBranch (l2, sub_mob2)) -> weightSum(sub_mob1) + weightSum(sub_mob2);;

  let rec balanced : mobile -> bool
  = fun mob ->
     match mob with
     | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if l1 * w1 == l2 * w2 then true else false
     | (CompoundBranch (l1, sub_mob1), SimpleBranch (l2, w2)) -> if balanced sub_mob1 && l1 * weightSum(sub_mob1) == l2 *w2 then true else false
     | (SimpleBranch (l1, w1), CompoundBranch (l2, sub_mob2)) -> if balanced sub_mob2 && l1 * w1 == l2 * weightSum(sub_mob2) then true else false
     | (CompoundBranch (l1, sub_mob1), CompoundBranch (l2, sub_mob2)) -> if balanced sub_mob1 && balanced sub_mob2 && l1 * weightSum(sub_mob1) == l2 * weightSum(sub_mob2) then true else false
end

open Problem2
let easy = (SimpleBranch (1, 4), CompoundBranch(2, (SimpleBranch(1, 1), SimpleBranch (1, 1))));;
weightSum easy;;
weightSum(SimpleBranch(1, 1), SimpleBranch (1, 1));;
balanced easy;;
let exam_2 = (CompoundBranch (3,
  (CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))),
     SimpleBranch (1, 4))),
      SimpleBranch (6, 3));;
balanced exam_2;;


(*
(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let calculator : exp -> int
  = fun exp -> raise NotImplemented  (* TODO *)
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> raise NotImplemented (* TODO *)
end

*)
