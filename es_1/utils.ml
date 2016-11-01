(*funzioni varie*)
open Syntax;;
open Printf;;

(*stampa il risultato della valutazinoe dell'espressione*)
let printlist l =	
	let rec p x= match x with
				 | Null				-> printf "\b"
				 | Cons(EInt(hd),tl)-> printf "%d " hd;
							   	   		p tl
				 | _				-> failwith "no integer element in integer list"
	in printf "-> [";
	   p l;
	printf "]\n"
;;

(* funzioni di supporto alla semantica*)
let rec s_lequals(l,r) = 
	match (l,r) with
	| (    Null         ,  Null      	  )	-> true
	| (   Cons(_,_)     ,  Null      	  )	-> false
	| (    Null         , Cons(_,_)       )	-> false
	| (Cons(EInt(lh),lt),Cons(EInt(rh),rt))	-> lh=rh && s_lequals(lt,rt)
	|	_									-> failwith "no integer element in integer list"
;;

let rec s_append(l,r) =
	match l with
	| Null 				-> r
	| Cons(EInt(hl),tl)	-> Cons(EInt(hl),s_append(tl,r))
	| _					-> failwith "no integer element in integer list"
;;

let rec s_compare(l,r) = 
	match (l,r) with
	| (    Null         ,  Null      	  )	-> true
	| (   Cons(_,_)     ,  Null      	  )	-> false
	| (    Null         , Cons(_,_)       )	-> true
	| (Cons(EInt(lh),lt),Cons(EInt(rh),rt))	-> lh=rh && s_compare(lt,rt)
	|	_									-> failwith "no integer element in integer list"
;;
(*
let rec s_member(e,l) =
	match l with
	| Null			  -> false
	| Cons(EInt(x),xs)-> e=x || s_member(e,xs)
	| _					-> failwith "no integer element in integer list"
;;

let rec s_compare(l,r) =
	match l with
	| Null			 	-> true
	| Cons(EInt(lh),lt) -> s_member(lh,r) && s_compare(lt,r)
	| _					-> failwith "no integer element in integer list"
;;

(*utile per il debug*)
let typecheck(v) =
	match v with
	| EInt(_)		-> printf "INT"
	| EBool(_)		-> printf "BOOL"
	| EList(_)		-> printf "LIST"
	| EFunval(_,_)	-> printf "FOO"
	| Novalue		-> printf "NOVALUE"
;;
*)
