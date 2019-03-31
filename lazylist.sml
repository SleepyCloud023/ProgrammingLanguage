datatype 'a lazyList = nullList
					| cons of 'a * (unit -> 'a lazyList)

fun seq(first : int, last : int) =
	if first <= last
	then cons(first,fn  = seq(first+1,last) )
	else nullList

fun infSeq(first : int) =
	cons(first,infSeq(first+1))

fun firstN(lazyListVal : lazyList, n :int) =
	if n > 0
	then
		case lazyListVal of 
			nullList x => []
		|	cons(head,tail) => head::firstN(tail,n-1)
	else []

fun Nth(lazyListVal : lazyList, n: int) =
	if n > 0
	then 
		case lazyListVal of
			nullList => NONE
		|   cons(head,tail) => 
						if n = 1
						then head
						else Nth(tail,n-1)
	else NONE





