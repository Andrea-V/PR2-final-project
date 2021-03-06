type 't env = string -> 't;;

exception WrongBindlist;;

(*crea un ambiente vuoto*)
let emptyenv(x) =
	function (y:string) -> x
;;

(*cerca un binding nell'ambiente*)
let applyenv(x,(y:string)) =
	x y
;;

(*aggiunge un'associazione all'ambiente*)
let bind((r: 'a env),(l:string),(e:'a)) = 
	function lu -> if lu = l then e 
							 else applyenv(r,lu)
;;

(* aggiunge una lista di binding all'ambiente*)
let rec bindlist(r,il,el) =
	match (il,el) with
	| 	 ([],[]) 	-> r
	| i::il1,e::el1 -> bindlist(bind(r,i,e),il1,el1)
	| 		_		-> raise WrongBindlist
;;

