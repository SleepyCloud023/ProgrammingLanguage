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
(* 패턴매칭을 이용해 valu 와 pattern 이 매치되는 경우만 처리합니다
* Tuple의 경우 foldl 함수를 이용해 매치된 nested valu, pattern 쌍에 대해서 string, valu
* list를 구하고 SOME 함수를 적용해서 반환합니다
* 명시되어 있지 않아서, 1번의 함수를 이용하여 주어진 패턴안의 변수가 모두 구분가능한지는 판단하지
* 않았습니다
* *)
fun match(v, p) =
  case (v,p) of 
       (_, Wildcard) => SOME []
     | (_ ,Variable s) => SOME ( (s,v)::[] )
     | (Unit, UnitP) => SOME []
     | (Const y,ConstP x) => if x = y
                             then SOME []
                             else NONE
     | (Tuple vs, TupleP ps) => 
         let 
           val pslen = List.length(ps)
           val vslen = List.length(vs)
         in (* ps 와 vs 의 길이가 같을때만 진행  *)
           if pslen <> vslen
           then NONE
           else
             let 
               val vps = ListPair.zip(vs,ps)
               val mat = List.filter (fn x => match(x) <> NONE) vps
               val matlen = List.length(mat)
             in (* 매치되는 경우만 필터한 배열의 길이가 그대로일때, 즉 모두
             매치될때의 경우만 진행  *)
               if pslen = matlen
               then SOME( foldl ( fn(x,y) => valOf(match(x)) @ y) [] vps )
               else NONE
             end
         end
     | (Constructor (s2,v), ConstructorP (s1,p) ) =>
         if s1 = s2 
         then match (v,p)
         else NONE
     | _ => NONE           
