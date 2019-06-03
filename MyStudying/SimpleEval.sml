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
	|	LESS(x1, x2) => if eval_expr(x1) < eval_expr(x2) 
						then true
						else false

(* test *)

val a = NUM 1
val b = NUM 2
val plus = PLUS(a,b)
val minus = MINUS(a,b)