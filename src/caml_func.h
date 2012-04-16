/**
		Ce fichier contiendra toutes les fonctions OCaml devant etre appellees
		par le C

		Il suffit de l inclure cote C pour profiter des fonctions
**/

/// fonction de test de main.ml
void caml_call_f(int x);

int fib(int n);

char * format_result(int n);
