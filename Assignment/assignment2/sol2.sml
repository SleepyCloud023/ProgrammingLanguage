(* Problem 1 *)
datatype expr = NUM of int
				| PLUS of expr * expr
				| MINUS of expr * expr

datatype formula = TRUE
				| FALSE
				| NOT of formula
				| ANDALSO of formula * formula
				| ORELSE of formula * formula
				| IMPLY of formula * formula
				| LESS of expr * expr
(*this function is used for eval LESS case*)
fun eval_expr( x: expr) = 
	case x of 
		NUM x => x
	|	PLUS(x1, x2) => (eval_expr(x1) + eval_expr(x2))
	|	MINUS(x1,x2) => eval_expr(x1) - eval_expr(x2)

fun eval( x: formula ) =
	case x of
		TRUE => true
	|	FALSE => false
	|	NOT x => not (eval x)
	|	ANDALSO(x1 ,x2) => (eval x1) andalso (eval x2) 
	|	ORELSE(x1, x2) => (eval x1) orelse (eval x2)
	(* x1 implies x2 is true when x2 is false or x1 and x2 are simultaneously true *)
	|	IMPLY(x1, x2) => (eval (NOT(x1))) orelse ((eval x1) andalso (eval x2))
	|	LESS(x1, x2) => eval_expr(x1) < eval_expr(x2) 
						

(* test *)
(*
val a = NUM 1
val b = NUM 2
val plus = PLUS(a,b)
val minus = MINUS(a,b)
*)
(* Problem 2 *)
type name = string
datatype metro = STATION of name
                |AREA of name * metro
                |CONNECT of metro * metro


fun checkMetro_list(areas : name list, x : metro) =
  let fun in_list(areas : name list, station : name) = 
      if null(areas) 
      then false 
      else (hd(areas) = station) orelse in_list(tl(areas),station) 
  in
  case x of  
    STATION name1 => in_list(areas,name1)
  | AREA (name1, metro1) => checkMetro_list(name1::areas, metro1)
  | CONNECT (metro1, metro2) => checkMetro_list(areas, metro1) 
                        andalso checkMetro_list(areas, metro2)
  end

fun checkMetro (x : metro) = 
  case x of
    STATION name1 => false
  | CONNECT (metro1, metro2) => checkMetro(metro1) andalso checkMetro(metro2)
  | AREA (name1, metro1) => checkMetro_list(name1::[],metro1) 


(*
fun checkMetro (x : metro) =
  case x of
      STATION x1 => false 
     |CONNECT (x1,x2) => checkMetro(x1) andalso checkMetro(x2)
     |AREA (x1, x2) => 
        if checkMetro(x2) = true
        then true
        else    
                case x2 of 
                        STATION t => (x1 = t)
                      | CONNECT(t1,t2) => 
                         checkMetro(AREA(x1,t1)) andalso checkMetro(AREA(x1,t2))
                      | AREA (t1,t2) =>  case t2 of
                                    STATION c => x1 = c
                                  | AREA(c1,c2) => checkMetro(AREA(x1,c2))
                                  | CONNECT(c1,c2) =>
                                            (checkMetro(AREA(x1,c1)) orelse
                                            checkMetro(AREA(t1,c1)))
                                            andalso (checkMetro(AREA(x1,c2))
                                            orelse checkMetro(AREA(t1,c2)))
*)                                                       
(* fail  *)
(*
val counter = AREA("a",AREA("b",AREA("c",AREA("d",
CONNECT(CONNECT(STATION"a",STATION"b"),CONNECT(STATION "c", STATION "d"))) )))  

val t = AREA("a", CONNECT(AREA("b", STATION "a"), AREA("c", AREA("a",
CONNECT(STATION "b", AREA("d", STATION "c"))))))

val Tcase = [AREA("a", STATION "a"), 
AREA("a", AREA("a",STATION("a"))),
AREA("a", AREA("b", CONNECT(STATION "a",STATION "b"))),
AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))]

val Fcase = [AREA("a", STATION "b"),
AREA("a", AREA("a", STATION "b")),
AREA("a",AREA("b",CONNECT(STATION "a", STATION "c"))),
AREA("a", CONNECT(STATION "a", AREA("b", STATION "C") ) )]


fun test( array : metro list) =
  if null array
  then true
  else 
    if checkMetro(hd array)
    then checkMetro(hd (tl array))
    else false
*)

(*Problem 3*)
datatype 'a lazyList = nullList
					| cons of 'a * (unit -> 'a lazyList)

fun seq(first : int, last : int) =
	if first <= last
	then cons(first,fn f => seq(first+1,last) )
	else nullList

fun infSeq(first : int) =
	cons(first,fn f => infSeq(first+1))

fun firstN(lazyListVal : 'a lazyList, n :int) =
	if n > 0
	then
		case lazyListVal of 
			nullList => []
		|	cons(head,tail) => head::firstN(tail(),n-1)
	else []

fun Nth(lazyListVal : 'a lazyList, n: int) =
	if n > 0
	then 
		case lazyListVal of
			nullList => NONE
		|   cons(head,tail) => 
						if n = 1
						then SOME(head)
						else Nth(tail(),n-1)
	else NONE

fun filterMultiples(lazyListVal : int lazyList, n : int) =
	if n > 0
	then
		case lazyListVal of 
			nullList => nullList
		|	cons(head,tail) => 
			if head mod n = 0
			then filterMultiples(tail(),n)
			else cons(head, fn f => filterMultiples(tail(),n))
	else nullList

fun sieve(lazyListVal : int lazyList) =
	case lazyListVal of 
		nullList => nullList
	|	cons(head,tail) => cons(head,fn f => (sieve(filterMultiples(tail(),head))) )

fun primes() = sieve(infSeq(2))




