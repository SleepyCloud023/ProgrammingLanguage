(* I want to know using copy without aliasing *)
fun append(xs : int list, ys : int list) =
	if null xs
	then ys
	else hd(xs) :: append(tl xs, ys)

fun merge(xs : int list, ys : int list) =
	if null xs
	then ys
	else 
		if hd xs < hd ys
		then merge(tl xs, hd xs::ys)
		else hd ys::merge(xs, tl ys)

fun reverse(xs : int list) =
	if null xs
	then []
	else append(reverse(tl xs), hd xs::[])

fun digits(x : int) =
	if x div 10 = 0
	then (x mod 10)::[]
	else append(digits(x div 10), x mod 10::[] )





