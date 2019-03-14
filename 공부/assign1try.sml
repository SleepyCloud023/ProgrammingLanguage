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


