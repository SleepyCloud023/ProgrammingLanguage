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
	|	IMPLY(x1, x2) => (eval x1 = false) orelse ((eval x1) andalso (eval x2))
	|	LESS(x1, x2) => if eval_expr(x1) < eval_expr(x2) 
						then true
						else false