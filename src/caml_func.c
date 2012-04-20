/**
		Ce fichier contiendra toutes les fonctions OCaml devant etre appellees
		par le C

		Il suffit de l inclure cote C pour profiter des fonctions
**/

#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

/// Fonction de test de main.ml
void call_caml_f(int x)
{
	static value * closure_f = NULL;
	if (closure_f == NULL) /* First time around, look up by name */
		closure_f = caml_named_value("Nom Arbitraire");

	caml_callback(*closure_f, Val_int(x));
}

int fib(int n)
{
  static value * fib_closure = NULL;
  if (fib_closure == NULL) fib_closure = caml_named_value("fib");
  return Int_val(caml_callback(*fib_closure, Val_int(n)));
}

char * format_result(int n)
{
  static value * format_result_closure = NULL;
  if (format_result_closure == NULL)
    format_result_closure = caml_named_value("format_result");
  return strdup(String_val(caml_callback(*format_result_closure, Val_int(n))));
  /* We copy the C string returned by String_val to the C heap
     so that it remains valid after garbage collection. */
}
