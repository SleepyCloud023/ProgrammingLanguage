(* sum the numbers in list for digitalRoot function *)
fun sum_list(xs : int list) = 
        if null xs
        then 0
        else hd xs + sum_list(tl xs)


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

fun sigma(a: int, b: int, f: int -> int ) = 
	if a < b
		then f(a) + sigma(a+1,b,f)
		else 
			if a = b
			then f(b)
			else ~1 (* if a > b case : exception *)

fun digits(x : int) =
	if x div 10 = 0
	then (x mod 10)::[]
	else append(digits(x div 10), x mod 10::[] )

fun digitalRoot(x: int) =
        if x div 10 > 0
        then digitalRoot( sum_list( digits(x) ) )
        else 
          if x div 10 = 0
          then x
          else ~1 (* if x is not positive integer *)
          
fun additivePersistence(x: int)=
        if x div 10 > 0
        then 1 + additivePersistence( sum_list( digits(x) ) )
        else 
          if x div 10 = 0
          then 0
          else ~1 (* if x is not positive integer  *)


