(**
    Les fonctions pratiques, appellees depuis tout le programme

    note : ne DOIT pas avoir de dependance a un autre fichier du programme
**)
open Batteries_uni

exception Invalid_int_of_char

(** retourne la valeur numerique d'un char numerique
    @param c:char un char digital tel '2'
    @return la valeur numerique de ce char
    @raise Invalid_int_of_char si le char n'est pas digital
**)
let int_of_char c =
  let code = (Char.code c) - 48 in
  if code >= 0 && code <= 9 then code
  else raise Invalid_int_of_char

exception Invalid_alphabet_char

(** retourne la place dnas l'alphabet d'un char alphabetique (A=0,B=1,etc.)
    @param c:char un char alphabetique tel 'A' ou 'a'
    @return la place dans l'alphabet latin de cette lettre
    @raise Invalid_alphabet_char si le char n'est pas une lettre de l'alphabet
**)
let int_of_letter c =
  let code = (Char.code c)- 65 in
  if code < 0 then raise Invalid_alphabet_char
  else if code < 27 then code
  else
    let code = code - 32 in
    if code < 0 then raise Invalid_alphabet_char
    else if code < 27 then code
    else raise Invalid_alphabet_char

let enum_push e elt = Enum.push e elt;e
