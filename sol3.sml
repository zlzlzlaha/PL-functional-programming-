datatype pattern  = Wildcard | Variable of string | UnitP
                 | ConstP of int | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list
              | Constructor of string * valu

(*test case
val w = Wildcard
val vp1 = Variable("1")
val vp2 = Variable("2")
val vp3 = Variable("3")
val up = UnitP
val cp1 = ConstP(1)
val cp2 = ConstP(2)
val cp3 = ConstP(3)
val tpp1 = TupleP([vp1,vp2,cp1,cp2,cp3,vp3,up,up])
val tpp2 = TupleP([Variable("99")])
val tpp3 = TupleP([tpp1,tpp2])
val ctp1 = ConstructorP("1",vp2)
val ctp2 = ConstructorP("1",vp3)

val c1 = Const(1)
val c2 = Const(2)
val c3 = Const(3)
val u = Unit
val ct1 = Constructor("1",u)
val ct2 = Constructor("2",u)
val tp1 = Tuple([c1,c2,c1,c2,c3,ct1,u,u])
val tp2 = Tuple([Unit])
val tp3 = Tuple([tp1,tp2])
*)



(*problem 1*)              
fun check_pat (p:pattern ) =
    let 
        (*help function to make string list in pattern*)
        fun get_name(p2 : pattern) =
            case p2 of  Variable p3 => p3 :: []
                     | TupleP(p_list) => List.foldl (fn (p4,ac) =>( get_name(p4)) @ ac ) [] p_list 
                     | ConstructorP(_,p3) => get_name p3 (*get nested pattern name*)
                     | _ =>[]
        (*help function to check same name in list*)              
        fun check_same(p_list : string list) =
             case p_list of [] => false
                | x::xs =>(List.exists (fn x1 => x1 = x ) xs) orelse check_same(xs)
        val name_list = get_name(p)
        val result = check_same(name_list)
       
    in
        not result 
    end


    
(*problem 2*)
fun match(v:valu , p:pattern) =
    let
        (*help function to get list length*)
        fun list_length xs = if null xs then 0 else 1 + list_length(tl xs)
        (*check tuple match and return binding list*)
        fun check_tuple (f, acc, xs) =  
            case xs of [] => SOME acc
                       |  x::xs1 => let 
                                        val element =  f(x)
                                    in
                                       if isSome element then check_tuple(f,( (valOf element ) @ acc ),xs1) else NONE
                                    end
    in
         case(v,p) of (_,Wildcard) => SOME []
            |  (v1 , Variable s) => SOME ((s,v1)::[]) 
            | (Unit,UnitP) => SOME []
            | (Const num1, ConstP num2) => if num1 = num2 then SOME [] else NONE
            | ( Tuple(t_list1),TupleP(t_list2)) =>  if list_length(t_list1) = list_length(t_list2)
                                                    then check_tuple ( match, [], (ListPair.zip(t_list1,t_list2)))
                                                    else NONE
            | (Constructor (s1,v1) ,ConstructorP(s2,p1)) => if s1 = s2 then match(v1,p1)  else NONE
            | _ => NONE
    end




type name = string
datatype RSP =
    ROCK
    | SCISSORS
    | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
    PLAYER of name * (RSP strategy ref)
    | MATCH of tournament * tournament

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two,one))
fun alterThree(one:RSP, two:RSP, three:RSP) = Cons(one, fn() => alterThree(two,three,one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK,PAPER)
val sr = alterTwo(SCISSORS,ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

(*test variable*)
(*
val p1 = PLAYER("one r ",ref r)
val p2 = PLAYER("one s ",ref s)
val p3 = PLAYER("one p ",ref p)
val p4 = PLAYER("rp ",ref rp)
val p5 = PLAYER("sr ",ref sr)
val p6 = PLAYER("ps ",ref ps)
val p7 = PLAYER("srp",ref srp)

val m1 = MATCH(p1,p2)
val m2 = MATCH(p2,p1)
val m3 = MATCH(p1,p3)
val m4 = MATCH(p3,p1)
val m5 = MATCH(p3,p2)
val m6 = MATCH(p2,p3)
val m7 = MATCH(p1,p4)
val m8 = MATCH(MATCH(p4,p6),MATCH(p5,p7)) 
val m9 = MATCH(p1,p4)
*)
fun next(strategyRef) = 
    let val Cons(rsp,func) = !strategyRef in
        strategyRef := func();
        rsp
     end
(*problem 3*)
fun whosWinner(t:tournament) =
    let
        (*help function to find winnder in each match*)
        fun check_match(p1: tournament , p2 : tournament) =
            let
                val PLAYER(name1,stg1) = p1
                val PLAYER(name2,stg2) = p2
                val rsp1 = next(stg1)
                val rsp2 = next(stg2)
            in
                case(rsp1,rsp2) of (ROCK,SCISSORS) => p1
                                | (ROCK,PAPER)=> p2
                                | (SCISSORS,PAPER)=> p1
                                | (SCISSORS,ROCK)=> p2
                                | (PAPER,ROCK) => p1
                                | (PAPER,SCISSORS) => p2
                                | (_,_) => check_match(p1,p2) 
            end                                                                                                                                 
    in
         case t of PLAYER(name,st) => PLAYER(name,st) 
                  | MATCH(t1,t2) =>check_match(whosWinner(t1),whosWinner(t2))
    end
