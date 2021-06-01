module Question1

module Question2
(* 2: Code Comprehension *)
    let rec foo =
        function 
        | [x]                 -> [x]
        | x::y::xs when x > y -> y :: (foo (x::xs))
        | x::xs               -> x :: foo xs

    let rec bar =
        function
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar (y :: xs)

    let rec baz =
        function
        | []               -> []
        | lst when bar lst -> lst
        | lst              -> baz (foo lst)
     

    //Q 2.1

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A:

    foo has type 'a list -> 'a list when 'a : comparison
    bar has type 'a list -> bool when 'a : comparison
    baz has type 'a list -> 'a list when 'a : comparison


    Q: What do functions ```bar```, and ```baz``` do 
       (not `foo`, we admit that it is a bit contrived)? 
       Focus on what they do rather than how they do it.

    A:
        bar takes a list and returns true if that list is ordered and false otherwise
        baz sorts a list in increasing order


    Q: What would be appropriate names for functions
       foo, bar, and baz?

    A: These three functions implement bubble sort. This is not something you are
        strictly required to know.

        foo could be called bubble (for bubble sort) or propagateLargest, or largestToEnd, or something similar.
        bar could be called isOrdered
        baz could be called sort or bubbleSort
    
    *)
        

    //Q 2.2

 
    (* 
    The functions foo and bar generate a warning during compilation: 
    'Warning: Incomplete pattern matches on this expression.' 
    
    Q: Why does this happen, and where? 

    A: Both foo and bar require that the argument lists are non-empty and the case where they
       are empty is not covered by the pattern match. The compiler warns that this case
       is not covered by either function.


    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: No it will not. The function baz checks if its input list is empty and it will never
       call either foo or bar with an empty list.

    *)

    let rec foo2 =
        function
        | []                  -> []
        | x::y::xs when x > y -> y :: (foo2 (x::xs))
        | x::xs               -> x :: foo2 xs

    let rec bar2 =
        function
        | []           -> true
        | [x]          -> true
        | x :: y :: xs -> x <= y && bar2 (y :: xs)

    (* Uncomment code to run after you have written foo2 and bar2 *)
    
    let rec baz2 =
      function
      | lst when bar2 lst -> lst
      | lst               -> baz2 (foo2 lst)
    

    //Q 2.3

    (* Consider this alternative definition of *)

    let rec foo3 =
      function 
      | [x]                 -> [x]
      | x::xs               -> x :: foo3 xs
      | x::y::xs when x > y -> y :: (foo3 (x::xs))

    (*

    Q: Do the functions `foo` and `foo3` produce the same output for all possible inputs? 
       If yes, why; if no why not and provide a counter example.

    A: No, they do not. By swapping the second and third row in the pattern match the guard x > y is never run
       as matches are always checked in order. The function foo3 will always return the same list that you give it.

       Counter example

       foo3 [9;1;2;3] = [9;1;2;3] <> [1;2;3;9] = foo [9;1;2;3]

    *)

    //Q 2.4

    let bar3 =
        function
        | [] -> true
        | x :: xs ->  
            List.fold (fun (b, x) y -> (b && x < y, y)) (true, x) xs |>
            fst

    //Q 2.5

    (*

    Q: The function foo or baz is not tail recursive. Which one and why?
    
    A: The function foo is not tail recursive as the following computation demonstrates

    foo [9;1;2;3] -->
    1 :: (foo (9::[2;3])) -->
    1 :: 2 :: (foo (9::[3])) -->
    1 :: 2 :: 3 :: (foo (9::[])) -->
    1 :: 2 :: 3 :: [9] -->
    [1;2;3;9]

    The cons-operators in every step build up and a value cannot be resolved until the function terminates.
    This can eventually lead to a stack overflow for large lists.

    *)

    (* ONLY implement the one that is NOT already tail recursive *)

    let fooTail lst =
        let rec aux c =
            function
            | [] -> c []
            | x :: y :: xs when x > y ->
                aux (fun result -> c (y :: result)) (x :: xs)
            | x :: xs ->
                aux (fun result -> c (x :: result)) xs

        aux id lst