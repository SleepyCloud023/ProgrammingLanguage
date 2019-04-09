datatype 'a lazyList = nullList
					| cons of 'a * (unit -> 'a lazyList)

fun seq(first : int, last : int) =
	if first <= last
	then cons(first,fn f => seq(first+1,last) )
	else nullList

fun infSeq(first : int) =
	cons(first,fn f => infSeq(first+1))
(* still loop when lazylist is shorter than n, how can this break out? *)
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




