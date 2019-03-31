type name = string
datatype metro = STATION of name
                |AREA of name * metro
                |CONNECT of metro * metro


fun checkMetro_list(areas : name list, x : metro) =
  let fun in_list(areas : name list, station : name) = 
      if null(areas) 
      then false 
      else (hd(areas) = station) orelse in_list(tl(areas),station) 
  in
  case x of  
    STATION name1 => in_list(areas,name1)
  | AREA (name1, metro1) => checkMetro_list(name1::areas, metro1)
  | CONNECT (metro1, metro2) => checkMetro_list(areas, metro1) 
                        andalso checkMetro_list(areas, metro2)
  end

fun checkMetro (x : metro) = 
  case x of
    STATION name1 => false
  | CONNECT (metro1, metro2) => checkMetro(metro1) andalso checkMetro(metro2)
  | AREA (name1, metro1) => checkMetro_list(name1::[],metro1) 


(*
fun checkMetro (x : metro) =
  case x of
      STATION x1 => false 
     |CONNECT (x1,x2) => checkMetro(x1) andalso checkMetro(x2)
     |AREA (x1, x2) => 
        if checkMetro(x2) = true
        then true
        else    
                case x2 of 
                        STATION t => (x1 = t)
                      | CONNECT(t1,t2) => 
                         checkMetro(AREA(x1,t1)) andalso checkMetro(AREA(x1,t2))
                      | AREA (t1,t2) =>  case t2 of
                                    STATION c => x1 = c
                                  | AREA(c1,c2) => checkMetro(AREA(x1,c2))
                                  | CONNECT(c1,c2) =>
                                            (checkMetro(AREA(x1,c1)) orelse
                                            checkMetro(AREA(t1,c1)))
                                            andalso (checkMetro(AREA(x1,c2))
                                            orelse checkMetro(AREA(t1,c2)))
*)                                                       
(* fail  *)
val counter = AREA("a",AREA("b",AREA("c",AREA("d",
CONNECT(CONNECT(STATION"a",STATION"b"),CONNECT(STATION "c", STATION "d"))) )))  

val t = AREA("a", CONNECT(AREA("b", STATION "a"), AREA("c", AREA("a",
CONNECT(STATION "b", AREA("d", STATION "c"))))))

val Tcase = [AREA("a", STATION "a"), 
AREA("a", AREA("a",STATION("a"))),
AREA("a", AREA("b", CONNECT(STATION "a",STATION "b"))),
AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))]

val Fcase = [AREA("a", STATION "b"),
AREA("a", AREA("a", STATION "b")),
AREA("a",AREA("b",CONNECT(STATION "a", STATION "c"))),
AREA("a", CONNECT(STATION "a", AREA("b", STATION "C") ) )]


fun test( array : metro list) =
  if null array
  then true
  else 
    if checkMetro(hd array)
    then checkMetro(hd (tl array))
    else false


