fun merge(xs:int list , ys: int list )=
    if null (xs) andalso null (ys)
    then [] 
    else if null (xs) 
         then ys
         else if null(ys)
               then xs
               else if hd(xs) <hd(ys)
                    then hd(xs):: merge(tl(xs),ys)
                    else hd(ys):: merge(xs,tl(ys))

(*reverse_help store each hd xs of each step, because  a first element of sub_list will be a last element of save_list *)
fun reverse(xs : int list) = 
    let fun reverse_help( sub_list : int list , save_list : int list) =
        if null sub_list (*when there is not element in sublist, return result*)
        then save_list
        else reverse_help(tl(sub_list),hd(sub_list)::save_list)
    in reverse_help(xs,[])
    end


(*a is from b is to*)
fun sigma(a:int , b : int , f : int -> int) =
    if a = b
    then f(b)
    else f(a) + sigma(a+1,b,f)

(*get most right number from input in each step, the result can be reversed , so define reverse function in digits *)  
fun digits(input_number : int ) =
    let 
        fun reverse_digits(number : int , save_list :int list ) = 
            let val portion = number div 10 val reminder = number mod 10
            in 
                if portion =0
                then reminder :: save_list
                else reverse_digits(portion,reminder::save_list)
            end
    in
        reverse_digits(input_number,[])
    end



(*if input is less than 10, then return 0*)
fun additivePersistence(number : int) = 
    let fun list_sum(numbers : int list) =
        if null numbers
        then 0
        else hd numbers + list_sum(tl numbers)
        val sum = list_sum(digits(number))
    in 
        if number < 10 
        then 0
        else 1+additivePersistence(sum)
    end

(*if sum of each numbers is less than 10, return it *)
fun digitalRoot(number : int) =
    let fun list_sum(numbers : int list ) =
        if null numbers
        then 0
        else hd numbers + list_sum(tl numbers)
        val sum = list_sum(digits(number))
    in
        if sum < 10
        then sum
        else digitalRoot(sum)
    end

