fun map_composed f g myList =
  case myList of
       [] => []
     | x::xs => f(g(x))::(map_composed f g xs)

val map_square = fn g =>  map_composed g (fn x => x * x)




