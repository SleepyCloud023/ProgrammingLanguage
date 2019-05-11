datatype pattern = Wildcard 
                 | Variable of string 
                 | UnitP
                 | ConstP of int | TupleP of pattern list
                 | ConstructorP of string * pattern
datatype valu =   Const of int 
                | Unit 
                | Tuple of valu list
                | Constructor of string * valu

fun check_pat pattern = 
let 
(* foldl을 이용해서 패턴안에 있는 모든 변수(문자열)을 리스트에 저장하여
* 반환해주는 함수  *)
fun listFromPattern P = 
  case P of  (* fn 이 받는 인자 첫번째는 foldl 의 리스트의 헤드로 두번째는
    accumulator 역할을 하는 인자로 정의되어 있습니다  *)
       TupleP plist => foldl( fn (x,y) => listFromPattern(x)@y) [] plist
     | Variable v => v::[]
     | ConstructorP (name, pattern) => listFromPattern(pattern)
     | _ => [] 
     (* 나머지 경우는 variable을 가지고 있지 않으므로 null list를 반환합니다 *)

(* 하나의 변수라도 중복되는것이 있으면 true 를 리턴 *)
fun duplicated (lst) =
  if null lst
  then false
  else (List.exists (fn x => x = hd lst) lst ) orelse (duplicated (tl lst))
in
  (* 두개의 도우미 함수를 이용합니다 *)
  not (duplicated(listFromPattern pattern) )
end
