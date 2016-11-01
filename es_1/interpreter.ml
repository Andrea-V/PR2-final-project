open Environment;;
open Syntax;;
open Utils;;
open Printf;;

(* semantica delle operazioni primitive *)
let p_and(b1,b2) =
	match (b1,b2) with
	| (EBool(bl),EBool(bf))	-> EBool(bl && bf)
	| _						-> failwith "type mismatch for 'and' operator"
;;

let p_or(b1,b2) =
	match (b1,b2) with
	| (EBool(bl),EBool(bf))	-> EBool(bl || bf)
	| _						-> failwith "type mismatch for 'or' operator"
;;

let p_not b =
	match b with
	| EBool(bv)	-> EBool(not bv)
	| _			-> failwith "type mismatch for 'not' operator"
;;

let p_add(e1,e2) = 
	match (e1,e2) with
	| (EInt(n1),EInt(n2))	-> EInt(n1 + n2)
	| _						-> failwith "type mismatch for '+' operator"
;;

let p_subtract(e1,e2) =
	match (e1,e2) with
	| (EInt(n1),EInt(n2))	-> EInt(n1 - n2)
	| _						-> failwith "type mismatch for '-' operator"
;;

let p_multiply(e1,e2) =
	match (e1,e2) with
	| (EInt(n1),EInt(n2))	-> EInt(n1 * n2)
	| _						-> failwith "type mismatch for '*' operator"
;;

let p_equals(e1,e2) =
	match (e1,e2) with
	| (EInt(n1),EInt(n2))	-> EBool(n1 = n2)
	| _						-> failwith "type mismatch for '=' operator"
;;

let p_lessequals(e1,e2) = 
	match (e1,e2) with
	| (EInt(n1),EInt(n2))	-> EBool(n1 <= n2)
	| _						-> failwith "type mismatch for '<=' operator"
;;

let p_compare(e1,e2) =
	match e1,e2 with
	| (EList(l1),EList(l2))	-> EBool(s_compare(l1,l2))
	| _						-> failwith "type mismatch for '<=' operator"
;;

let p_lequals(e1,e2) =
	match (e1,e2) with
	| (EList(l1),EList(l2))	-> EBool(s_lequals(l1,l2))
	| _						-> failwith "type mismatch for '==' operator"
;;

let p_isempty e =
	match e with
	| EList(l)	->	begin match l with
					| Null	-> EBool(true)
					| _		-> EBool(false)					
					end
	| _			-> failwith "type mismatch for 'isEmpty' operator"
;;

let p_append(e1,e2) =
	match (e1,e2) with
	| (EList(l1),EList(l2))	-> EList(s_append(l1,l2))
	| _						-> failwith "type mismatch for '@' operator"
;;

let rec p_map(f,l,env) =
	match l with
	| Null				-> l
	| Cons(EInt(hd),tl)	-> let res = semexp(Apply(f,Val(EInt(hd))),env) in
							Cons(res,p_map(f,tl,env))
	| _					-> failwith "no integer element in integer list"

(*semantica delle espressioni*)
and semexp(e,env) = 
	match e with
	| Ide(id)		-> dvaltoeval(applyenv(env,id))
	| Val(v)		-> v
	| And(b1,b2)	-> p_and	(semexp(b1,env),semexp(b2,env))
	| Or (b1,b2)	-> p_or		(semexp(b1,env),semexp(b2,env))
	| Not(b)		-> p_not	(semexp(b,env))
	| Op(op,e1,e2)	-> begin match op with
					   | "+"	-> p_add	   (semexp(e1,env),semexp(e2,env))
					   | "-"	-> p_subtract  (semexp(e1,env),semexp(e2,env))
					   | "*"	-> p_multiply  (semexp(e1,env),semexp(e2,env))
					   | "="	-> p_equals    (semexp(e1,env),semexp(e2,env))
					   | "<="	-> p_lessequals(semexp(e1,env),semexp(e2,env))
					   | _		-> failwith "unknown operation"
					   end
	| Compare(e1,e2)-> p_compare(semexp(e1,env),semexp(e2,env))
	| Equals (e1,e2)-> p_lequals(semexp(e1,env),semexp(e2,env))
	| IsEmpty(l)	-> p_isempty(semexp(l,env))
	| Append (e1,e2)-> p_append	(semexp(e1,env),semexp(e2,env))	
	| Map(f,l)		-> begin match semexp(l,env) with
					   | EList(li) 	-> EList(p_map(f,li,env))
					   | _			-> failwith "type mismatch for 'map/on' operator"
					   end
	| If(g,th,el)	-> begin match semexp(g,env) with
					   | EBool(true)	-> semexp(th,env)
			  		   | EBool(false)	-> semexp(el,env)
					   | _				-> failwith "'if' construct need a boolean guard"
					   end	
	| Let(id,rh,b)	-> let rhv = semexp(rh,env) in
					   begin match rhv with
					   | EFunval(arg,body)	-> let env1 = bind(env,id,DClosure(arg,body,env)) in
												semexp(b,env1)
					   | _					-> let env1 = bind(env,id,evaltodval(rhv)) in
												semexp(b,env1)
					   end
	| Apply(foo,apar)-> begin match foo with 							
						| Ide(f)	-> begin match applyenv(env,f) with	(*foo gia' definita*)
										| DClosure(fpar,body,stenv) -> let stenv1 = semarg(fpar,apar,env,stenv) in																		
																		semexp(body,stenv1)
										| _						    -> failwith "no function to apply"
									   end
						| _			-> begin match semexp(foo,env) with (*foo definita sul momento*)
										| EFunval(fpar,body)		->	let env1 = semarg(fpar,apar,env,env) in																		
																		 semexp(body,env1)
										| _							->	failwith "no function to apply"
									   end
						end

(* semantica dei parametri *)
and semarg(fp,ap,env,stenv) =
	match ap with
	| Ide(f)	-> bind(stenv,fp,applyenv(env,f))
	| _			-> let apv = semexp(ap,env) in
					begin match apv with 
					| EFunval(arg,body)	-> bind(stenv,fp,DClosure(arg,body,env))
					| _					-> bind(stenv,fp,evaltodval(apv))
					end
;;
