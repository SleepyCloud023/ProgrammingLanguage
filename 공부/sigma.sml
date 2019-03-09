fun pow( x: int, y: int) =
	if y=0
	then 1
	else x * pow(x,y-1)

fun square( x: int) =
	x*x

fun equal( x: int) =
	x

fun sigma( x: int , y: int , f ) =
	if x=y
	then f(x)
	else f(x) + sigma(x+1,y,f)




