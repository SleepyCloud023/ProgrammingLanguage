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
  case lst of 
    [] => true
  | x::[] => false
  | x::xs =>
      (List.exists (fn t => t = x) xs) orelse duplicated xs
 (*
  if null lst
  then true
  else (List.exists (fn x => x = hd lst) (tl lst)) orelse (duplicated (tl lst))
 *)
in
  (* 두개의 도우미 함수를 이용합니다 *)
  not (duplicated(listFromPattern pattern)) 
end
(* 패턴매칭을 이용해 valu 와 pattern 이 매치되는 경우만 처리합니다
* Tuple의 경우 foldl 함수를 이용해 매치된 nested valu, pattern 쌍에 대해서 string, valu
* list를 구하고 SOME 함수를 적용해서 반환합니다
* 따로 명시되어 있지 않아서, 1번의 함수를 이용하여 주어진 패턴안의 Variable이 모두
* 구분가능한지는 판단하지는 않았습니다
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
(*
val pat1 = Variable("hi")
val pat2 = Variable("hi")
val pat3 = TupleP([pat1,pat2])
val value1 = Const(17)
val value2 = Const(15)
val value3 = Tuple([value1,value2])
*)
type name = string
datatype RSP = ROCK
             | SCISSORS
             | PAPER
datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament = PLAYER of name * (RSP strategy ref)
                    | MATCH of tournament * tournament

fun onlyOne( one: RSP ) = Cons(one, fn() => onlyOne(one))
fun alterTwo( one: RSP, two : RSP ) = Cons( one, fn() => alterTwo(two, one))
fun alterThree( one : RSP, two : RSP, three : RSP ) = Cons(one, fn() =>
  alterThree( two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)


(* tournamnet를 인자로 받아서 이긴 플레이어를 반환
* 도우미 함수들을 let in end로 
* *)
fun whosWinner(t) = 
let 
  (* 함수 next는현재 rsp 를 반환하고 다음 상태로 ref를 업데이트하는 함수 *)
  fun next(strategyRef) =
  let val Cons(rsp, func) = !strategyRef 
  in 
    strategyRef := func();
   (* 이 rsp는 func()를 통해 업데이트되기전의 값이므로 현재 rsp 정보임에 유의 *)
    rsp
  end

  (* rsp 두개를 받아 이기는 rsp option 값을 반환해주는 함수
   비기는 경우에 NONE 을 반환함 *)
  fun win_rsp (r1, r2) =
    case r1 of 
         ROCK => if r2 = ROCK
                 then NONE
                 else 
                   if r2 = SCISSORS
                   then SOME r1
                   else SOME r2
       | SCISSORS => if r2 = SCISSORS
                     then NONE
                     else 
                       if r2 = PAPER
                       then SOME r1
                       else SOME r2
       | PAPER => if r2 = PAPER
                  then NONE
                  else 
                    if r2 = ROCK
                    then SOME r1
                    else SOME r2
  (* 이긴 플레이어를 반환*)
  fun winner_one (p1, p2) =
    case (p1,p2) of 
         (PLAYER (name1,str1), PLAYER (name2, str2)) =>
         let (* 현재 rsp 정보를 받아와서 승자를 확인 *)
           val rsp1 = next(str1)
           val rsp2 = next(str2)
           val winner = win_rsp(rsp1,rsp2)
         in
           if winner = NONE
           then (* 비기는 경우에는 한번 더 진행  *)
             winner_one(p1,p2)
           else (* 승패가 난 경우에 승자 플레이어를 반환 *)
             if valOf(winner) = rsp1
             then p1
             else p2
         end
         (* p1 p2가 모두 플레이어가 되도록 전처리하여 사용하므로 예외처리  *)
       | _ => PLAYER("exception", ref r)

in
  case t of 
       MATCH (p1, p2) =>
       let 
         val win_p1 = whosWinner(p1) 
         val win_p2 = whosWinner(p2) 
       in
         winner_one(win_p1,win_p2)
       end             
     | PLAYER p => t
end

(* test *)
(*
val S = PLAYER("s", ref s)
val R = PLAYER("r", ref r)
val P = PLAYER("p", ref p)
val RP = PLAYER("rp", ref rp)
val SR = PLAYER("sr", ref sr)
val PS = PLAYER("ps", ref ps)
val SRP = PLAYER("SRP",ref srp)

val f = whosWinner
fun m (a,b) = MATCH(a,b)

val winner_abc = whosWinner(MATCH(S,MATCH(RP,R)))
*)


