(**
Les fonctions pratiques, appellees depuis tout le programme

note : ne DOIT pas avoir de dependance a un autre fichier du programme
**)
open Batteries_uni

exception Quit_signal


(** retourne la valeur numerique d'un char numerique
@param c: char un char digital tel '2'
@return la valeur numerique de ce char
**)
let int_of_char c =
  let code = (Char.code c) - 48 in
  (assert (code >= 0);
    assert (code <= 9);
    code)

(** retourne la place dans l'alphabet d'un char alphabetique (A=0,B=1,etc.)
@param c: char un char alphabetique tel 'A' ou 'a'
@return la place dans l'alphabet latin de cette lettre
**)
let int_of_letter c =
  let code = (Char.code c) - 65 in
  (assert (code >= 0);
  if code < 27 then code
  else
    let code = code - 32 in
    (assert (code >= 0);
     assert (code < 17);
     code))

exception Invalid_alphabet_of_int

let letter_of_int i =
  let c = Char.chr (i +65) in
  match c with
  | 'A'..'Z' | 'a'..'z' -> c
  | _ -> raise Invalid_alphabet_of_int

let enum_push e elt = BatEnum.push e elt; e

let drop_one e = BatEnum.drop 1 e; e

(* fonction de switch sans test (dans predicate), et success non-parametré *)
let rec switchF (default:'a) (predicates: ((('b -> bool) * 'a) list)) (param:'b) =
  match predicates with
  | [] -> default param
  | (test, success)::[] -> if test param then success param
      else default param
  | (test, success):: l -> if test param then success param
      else switchF default l param

(* fonction de switch avec comparer parametré, et success parametré *)
let rec switchC (comparer: ('a -> 'b -> bool)) (default:'b) predicates (param:'b) =
  match predicates with
  | [] -> default
  | (test, success)::[] -> if comparer test param then success param
      else default
  | (test, success):: l -> if comparer test param then success param
      else switchC comparer default l param

(* fonction de switch avec comparer prédéterminé (=), et success paramétré *)
let rec switch = (switchC (=))
let a = switch 0 [(1, ((+) 2))] 2

let div_eucl a b =
  let rec div a b q = if a < b then (q, a) else div (a - b) b (q +1)
  in div a b 0

let bitset_set_nth b n =
   let b = BatBitSet.copy b in (BatBitSet.put b true n;b)
let bitset_unset_nth b n =
   let b = BatBitSet.copy b in (BatBitSet.put b false n;b)
