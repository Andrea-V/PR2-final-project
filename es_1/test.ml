open Syntax;;
open Environment;;
open Interpreter;;
open Printf;;
open Utils;;

(*stampa il risultato di valutazione dell'espressione*)
let printresult(e,env) = 
	match semexp(e,env) with
	| EInt(n)		-> printf "-> %d\n" n 
	| EBool(b)		-> printf "-> %b\n"	b
	| EList(l)		-> printlist l 
	| EFunval(_)	-> printf "-> <fun>\n"
	| Novalue		-> printf "-> 'novalue'\n"
;;
printf "*-----------------------------------*\n";;
printf "::TEST::\n\n";;

let _1 = Val(EInt(1));;
let _2 = Val(EInt(2));;
let _3 = Val(EInt(3));;
let _4 = Val(EInt(4));;
let _5 = Val(EInt(5));;
let _6 = Val(EInt(6));;
let _7 = Val(EInt(7));;
let _8 = Val(EInt(8));;
let _9 = Val(EInt(9));;
let _0 = Val(EInt(0));;

let l1 = Cons(EInt(1),Cons(EInt(2),Cons(EInt(3),Null)));;
let l2 = Cons(EInt(5),Cons(EInt(0),Cons(EInt(7),Null)));;
let l3 = Cons(EInt(0),Cons(EInt(7),Null));;
let l4 = Cons(EInt(5),Null);;
let l5 = Null;;

let el1 = Val(EList(l1));;
let el2 = Val(EList(l2));;
let el3 = Val(EList(l3));;
let el4 = Val(EList(l4));;
let el5 = Val(EList(l5));;

let _false = Val(EBool(false));;
let _true  = Val(EBool(true ));;
	
let stdenv = emptyenv(Unbound);;


printf "\n- prova operatori logici:";;
let exp1 = And(_true,Or(_false,Not(_false)));;
printf "\n1.\n true && (false || not(false) ) :";;
printresult(exp1,stdenv);;


printf "\n- prova operatori aritmetici:";;
let exp2 = Op("+",_3,Op("*",Op("-",_3,_1),_6));;
printf "\n2.\n (3+((3-1)*6)) :";;
printresult(exp2,stdenv);;


printf "\n- prova operatori su liste:";;
let exp3 = Equals(Append(el4,el3),el2);;
printf "\n3.\n [5] @ [0 7] == [5 0 7] :";;
printresult(exp3,stdenv);;

let exp4 = Equals(Append(el3,el4),el2);;
printf "\n4.\n [0 7] @ [5] == [5 0 7] :";;
printresult(exp4,stdenv);;

let exp5 = Compare(el3,el2);;
printf "\n5a.\n [0 7] <= [5 0 7] :";;
printresult(exp5,stdenv);;

let exp19 = Compare(el4,el2);;
printf "\n5b.\n [5] <= [5 0 7] :";;
printresult(exp19,stdenv);;


printf "\n- prova if:";;
let exp6 = If(Op("<=",_5,Op("*",_2,_3)),IsEmpty(el5),IsEmpty(el4));;
printf "\n6.\n if 5 <= (2*3) \n    then isEmpty([]) \n    else isEmpty([5])\n :";;
printresult(exp6,stdenv);;

let exp7 = If(Op("<=",_8,Op("*",_2,_3)),IsEmpty(el5),IsEmpty(el4));;
printf "\n7.\n if 8 <= (2*3) \n    then isEmpty([]) \n    else isEmpty([5])\n :";;
printresult(exp7,stdenv);;

let x = Ide("x");;
let y = Ide("y");;
let z = Ide("z");;
let f = Ide("f");;

let iplus1	= Ide("plus1");;
let plus1_z = Val(EFunval("z",Op("+",z,_1)));;

let iide	= Ide("ide");;
let ide_y	= Val(EFunval("y",y));;

let ifoo	= Ide("foo");;
let foo_y	= Val(EFunval("y",x));;


printf "\n- prova let:";;
let exp8 = Let("x",_3,Let("y",Op("+",x,_1),Op("*",x,y)));;
printf "\n8.\n let x=3 in\n let y=x+1 in\n  x*y \n :";;
printresult(exp8,stdenv);;

let exp9 = Let("x",el4,Let("y",el1,If(Compare(x,y),y,Append(x,y))));;
printf "\n9.\n let x=[5] in\n let y=[1 2 3] in\n  if x<=y \n   then y \n   else x @ y\n :";;
printresult(exp9,stdenv);;


printf "\n- prova funzioni:";;
let exp10 = Let("plus1",plus1_z,Op("*",Apply(iplus1,_1),Apply(iplus1,Op("-",_5,_2))));;
printf "\n10.\n let plus1 = fun{z -> z+1} in\n  plus1(1) * plus1(5-2) \n :";;
printresult(exp10,stdenv);;

let exp11 = Let("plus1",plus1_z,Op("*",Apply(iplus1,_1),Apply(Val(EFunval("y",Op("+",y,_2))),_3)));;
printf "\n11.\n let plus1 = fun{z -> z+1} in\n  plus1(1) * fun{y -> y+2}(3) \n :";;
printresult(exp11,stdenv);;

printf "\n- prova map:";;
let exp12 = Let("x",el1,Let("plus1",plus1_z,Map(iplus1,x)));;
printf "\n12.\n let x = [1 2 3] in\n let plus1 = fun{z -> z+1} in\n  map plus1 on x\n :";;
printresult(exp12,stdenv);;

let exp13 = Let("plus1",plus1_z,Map(Val(EFunval("z",Op("*",z,_2))),el1));;
printf "\n13.\n let x = [1 2 3] in\n  map fun{z -> z*2} on x\n :";;
printresult(exp13,stdenv);;


printf "\n- prova scope:";;
let exp14 = Let("x",_3,Let("y",_5,Let("x",_1,Op("+",y,Op("*",x,_2)))));;
printf "\n14.\n let x=3 in \n let y=5 in\n let x=1 in\n  y+x*2\n :";;
printresult(exp14,stdenv);;

let exp15 = Let("x",_3,Let("foo",foo_y,Let("x",_2,Apply(ifoo,x))));;
printf "\n15.\n let x=3 in \n let foo = fun{y -> x} in\n let x=2 in\n  foo(x)\n :";;
printresult(exp15,stdenv);;

let exp16 = Let("x",_3,Let("ide",ide_y,Let("x",_2,Apply(iide,x))));;
printf "\n16.\n let x=3 in \n let ide = fun{y -> y} in\n let x=2 in\n  ide(x)\n :";;
printresult(exp16,stdenv);;

printf "\n- prova funzioni di ordine superiore:";;
let ihord	= Ide("hord");;
let ihord2	= Ide("hord2");;
let iminus1	= Ide("minus1");;	

let hord_f	= Val(EFunval("f",Op("*",Apply(f,_5),_3)));;
let hord2_x = Val(EFunval("x",If(Not(x),Val(EFunval("z",Op("-",z,_1))),Val(EFunval("z",Op("+",z,_1))))));;

let exp17 = Let("hord",hord_f,Let("plus1",plus1_z,Apply(ihord,iplus1)));;
printf "\n17.\n let hord = fun{f -> f(5)*3} in\n let plus1 = fun{z -> z+1} in\n  hord(plus1)\n :";;
printresult(exp17,stdenv);;

let exp18 = Let("hord2",hord2_x,Let("minus1",Apply(ihord2,_false),Let("plus1",Apply(ihord2,_true),Op("-",Apply(iplus1,_2),Apply(iminus1,_4)))));;
printf "\n18.\n let hord2 = fun{x -> if !x then fun{z -> z-1} else fun{z-> z+1}} in\n let minus1 = hord2(false) in\n \
        let plus1 = hord2(true) in\n  plus1(2) - minus1(4)\n :";;
printresult(exp18,stdenv);;

