datatype expr = NUM of int
                | PLUS of expr * expr
                | MINUS of expr * expr

datatype formula = TRUE
                  | FALSE
                  | NOT of formula
                  | ANDALSO of formula * formula
                  | ORELSE of formula* formula
                  | IMPLY of formula * formula
                  | LESS of expr * expr

(*formula -> formula this function evaluate any formula into  TRUE or FALSE*)
fun eval_fm (f : formula) = 
    (*expr -> int, evaluate any expr into result's integer*)
    let fun eval_expr (e : expr) = 
        case e of  NUM i => i
                  |PLUS(e1,e2) =>(eval_expr e1) + (eval_expr e2)
                  |MINUS(e1,e2) => (eval_expr e1) - (eval_expr e2)
    in
        case f of 
                 TRUE => TRUE
                 |FALSE => FALSE 
                 |NOT fm => if (eval_fm fm) = TRUE then FALSE else TRUE
                 |ANDALSO(fm1,fm2) => if (eval_fm fm1) = TRUE then  (eval_fm fm2) else FALSE
                 |ORELSE(fm1,fm2) => if (eval_fm fm1) = FALSE then  (eval_fm fm2)  else TRUE
                 |IMPLY (fm1,fm2) =>  if (eval_fm fm1) = TRUE then (eval_fm fm2) else TRUE
                 |LESS (e1,e2) =>  if (eval_expr e1 ) < (eval_expr e2) then TRUE else FALSE
    end
                  
(*formula -> bool , only check TRUE or FALSE and return boolean result *)                 
fun eval (fm : formula)  =
    (*get fomula type which is TRUE or FALSE*)
    let val  f = eval_fm(fm) 
    in    
        case f  of  TRUE => true
                    | FALSE => false
    end
             

type name = string
datatype metro = STATION of name
                | AREA of name * metro
                | CONNECT of metro * metro


(*metro -> string list*)
(*when we meet STATION, add its name in empty list*)
(*when we meet AREA, delete the name which is same with area name in list*)
(*when we meet CONNECT, append two string lists*)
fun evalMetro (mt : metro) =
    (*string * string list -> string list ,,,  find same name and delete it from string list*)
    let fun checkName(n : string, xs : string list) =
            case xs of [] => []
                       | xs1 :: [] => if xs1 = n then [] else xs1::[]
                       | xs1 :: rest => if xs1 = n then checkName(n,rest) else xs1::checkName(n,rest) 
    in 
            case mt of STATION n=> n :: []
                        |AREA(n,met) => checkName(n, evalMetro(met))
                        |CONNECT(met1,met2) => (evalMetro met1 ) @  (evalMetro met2)
    end

(*metro -> bool , eval metro and check this metro's string list is empty or not*)
fun checkMetro (mt : metro) = 
    let val name_list = evalMetro(mt)
    in
         if null name_list then true else false
    end
                


datatype 'a lazyList = nullList
                     | cons of  'a * (unit -> 'a lazyList)

(* int * int -> int lazyList *)
fun seq(first : int , last : int) =
    if first <= last  
    then  cons(first, fn() => seq(first+1,last)) 
    else nullList

 (* int -> int lazyList *)
fun infSeq(first : int) = 
    cons(first,fn() => infSeq(first+1))

(* 'a lazyList * int -> 'a list*)
fun firstN(lazyListVal : 'a lazyList, n : int)  =
    case lazyListVal of nullList => []
                       | cons(i,f) => if n > 0 then (i :: firstN(f(),n-1)) else []
(*  'a lazyList * int -> 'a option *)
fun Nth(lazyListVal : 'a lazyList, n : int) =
    case lazyListVal of nullList =>NONE
                     | cons(i,f) => if n > 1 
                                    then Nth(f(),n-1) 
                                    else if n = 1 then SOME(i) else NONE
(*int lazyList * int -> int lazyList*)
fun filterMultiples(lazyListVal : int lazyList, n : int ) =
    case lazyListVal of nullList => nullList
                    | cons(i,f) => if (i mod n) = 0 then filterMultiples(f(),n) else cons(i,fn()=>filterMultiples(f(),n))
  
(*int lazyList -> int lazyList*)
fun sieve(lazyListVal : int lazyList) = 
    case lazyListVal of nullList => nullList
                    | cons(i,f) => cons(i,fn()=>sieve(filterMultiples(f(),i)))

(* () -> int lazyList *)
fun primes() =  sieve(infSeq(2))

 
