(**
pour l'instant, cette page ne contient pas de code, juste des constantes du go
qu il faut garder en tete
**)

(* POSITIONNEMENT*)
(* ligne 3: la ligne du territoire*)
(* ligne 4: la ligne de la victoire (ou de l influence)*)
(* ligne 2: la ligne de la defaite *)

(* VIE ET MORT (nous sommes noirs) *)
(* - un groupe de 3 peut etre tué: si le groupe est dans un coin, il meurt*)
(*                                 au bord, si blanc joue au milieu, noir meurt*)
(* - 4 dans une boite meurt*)
(* - 4 dans une ligne droite vit (avec un zig-zag marche aussi) *)
(* - 4 dans une pyramide vit si blanc ne joue pas premier au milieu*)
(* - 5 vit tout le temps ou presque, pareil pour 6 et plus *)
