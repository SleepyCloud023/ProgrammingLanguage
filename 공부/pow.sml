(* power function x^y *)
(* Note: correct only if y>=0 *)
fun pow (x : int, y : int ) =
	if y=0
	then 1
	else x * pow(x,y-1)

(* x x x x x x x x pow(x,y-n)
   equal
   x^n pow(x,0)     


*)