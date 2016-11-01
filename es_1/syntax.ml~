(* grammatica e sintassi linguaggio *)
open Environment;;

exception Inexpressible;;

(*grammatica*)
type ide 	= string

and opcod	= string

and intlist	=
	| Null
	| Cons of eval * intlist

and exp =
	| Ide 		of ide
	| Val 		of eval
	| And 		of exp * exp
	| Or  		of exp * exp
	| Not 		of exp
	| Op		of opcod * exp * exp
	| Compare	of exp * exp
	| Equals	of exp * exp
	| IsEmpty	of exp
	| Append	of exp * exp
	| If		of exp * exp * exp
	| Let		of ide * exp * exp
	| Apply		of exp * exp
	| Map		of exp * exp

(* valori esprimibili *)
and eval = 
	| EInt		of int
	| EBool		of bool
	| EList		of intlist
	| EFunval	of ide * exp
	| Novalue

(* valori denotabili *)
and dval = 
	| DInt		of int
	| DBool		of bool
	| DList		of intlist
	| DClosure	of ide * exp * dval env
	| Unbound
;;

(* funzioni di conversione eval<->dval *)
let evaltodval(e:eval):dval = 
	match e with
	| EInt(i)			-> DInt(i)
	| EBool(b)			-> DBool(b)
	| EList(l)			-> DList(l)
	| Novalue			-> Unbound
	| _					-> raise Inexpressible
;;

let dvaltoeval(d:dval):eval = 
	match d with
	| DInt(i)			-> EInt(i)
	| DBool(b)			-> EBool(b)
	| DList(l)			-> EList(l)
	| Unbound			-> Novalue
	| _					-> raise Inexpressible
;;

