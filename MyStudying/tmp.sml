fun test xs = 
let 
  val a = map rev
  fun t x = a x
in
 t xs
end 
