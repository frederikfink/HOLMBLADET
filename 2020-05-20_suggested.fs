module Question1
    //Q 1.1
    let rec insert x =
        function
        | [] -> [x]
        | y :: ys when y < x -> y :: (insert x ys)
        | y :: ys            -> x :: y :: ys

    let rec insertionSort =
        function
        | [] -> []
        | x :: xs -> insert x (insertionSort xs)

    //Q 1.2

    let insertTail x = 
        let rec aux acc =
            function
            | [] -> (x :: acc) |> List.rev
            | y :: ys when y < x -> aux (y :: acc) ys
            | y :: ys            -> (List.rev (y :: x :: acc)) @ ys

        aux []

    let insertionSortTail lst =
        let rec aux acc =
            function
            | [] -> acc
            | x :: xs -> aux (insertTail x acc) xs

        aux [] lst

    //Q 1.3

    (* 
    Q: Why are the higher-order functions from the List library 
    not a good fit to implement insert?

    A: Because higher-order functions from the list library typically go through the entire list whereas insert
       terminates as soon as it has found the insertion point. It is possible to use
       these higher-order functions to write insert, it is just not practical.

       A few examples that use higher-order functions to code insert follow, 
       but are in no way required for the exam.
    *)

    let insertFilter x lst = List.filter ((>=) x) lst @ x :: (List.filter ((<) x)) lst
    let insertFold x lst =
        List.foldBack
            (fun y -> 
                function 
                | (true, acc) when x >= y -> (false, y :: x :: acc)
                | (cont, acc)             -> (cont, y :: acc))
            lst
            (true, []) |>
            snd

    let insertFold2 x lst =
        List.foldBack
            (fun y (cont, acc) -> 
                if cont && x >= y then 
                    (false, y :: x :: acc)
                else 
                    (cont, y :: acc))
            lst
            (true, []) |>
            snd


    let insertionSort2 lst = List.foldBack insertTail lst []

    //Q 1.4

    let insertBy f x = 
        let rec aux acc =
            function
            | [] -> (x :: acc) |> List.rev
            | y :: ys when f y < f x -> aux (y :: acc) ys
            | y :: ys            -> (List.rev (y :: x :: acc)) @ ys

        aux []

    let insertionSortBy f lst = List.foldBack (insertBy f) lst []

module Question2

    //Q 2.1
    let rec foo x = 
        function 
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)

    let rec bar x =
        function
        | []        -> []
        | xs :: xss -> (x :: xs) :: bar x xss 

    let rec baz =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> 
            let rec aux =
                function
                | []      -> []
                | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)
            aux xs

    (* 
    
    Q: What are the types of functions foo,  bar, and baz?

    A:  foo : 'a -> 'a list -> 'a list when 'a : equality
        bar : 'a -> 'a list list -> 'a list list
        baz : 'a list -> 'a list list when 'a : equality


    Q: What do functions foo, bar, and baz do? 

    A: foo takes an element `x` and a list `lst` and removse the first occurrence of `x` in `lst`
       bar taks an element `x` and a list of lists `lst` and adds `x` to the front of all lists in `lst`
       baz takes a list `lst` and returns a list containing all possible permutations of `lst`


    Q: What would be appropriate names for functions 

    A: foo: removeFirstOccurrence
       bar: addToAll
       baz: permutations


    *)        


    //Q 2.2
    (* 
    The function foo generates a warning during compilation: 
    Warning: Incomplete pattern matches on this expression.

    
    Q: Why does this happen, and where? 

    A: It happens because foo does not have a case that matches the empty list.
       In particular, this case will be reached whenever we try to remove an element from a list
       that is not in that list.

    Q: For these particular three functions will this incomplete 
       pattern match ever cause problems for any possible execution of baz? 
       If yes, why; if no, why not.

    A: No. The empty list case for foo will only ever be called if you try to remove an element
       from a list that does not exist in the list. The internal function aux of baz recurses over a list
       and removes elements from that list using foo - it will never attempt to remove an element from a list
       that does not exist in that list.


    *)


    let rec foo2 x = 
        function 
        | []                 -> []
        | y :: ys when x = y -> ys
        | y :: ys            -> y :: (foo x ys)


    //Q 2.3

    (* 
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression

    A: 'a list -> 'a list list when 'a : equality


    Q: What does it do? Focus on what it does rather than how it does it.

    A: The function takes a list `lst` and returns all possible permutations of `lst` that start with `y`.

    *)

    //Q 2.4

    let bar2 x lst = List.map (fun xs -> x :: xs) lst

    //Q 2.5

    let rec baz2 =
        function
        | [] -> []
        | [x] -> [[x]]
        | xs  -> List.foldBack (fun y acc -> (foo y >> baz >> bar y) xs @ acc) xs []

    //Q 2.6

    (*
    
    Q: The function foo is not tail recursive. Why?

    A: The function foo is not called in a tail-recursive position and a string of cons-operators will
       be generated that cannot be reduced until the base case is called.

       foo 3 [1;2;3;3;4;5] ->
       1 :: (foo 3 [2;3;4;5]) ->
       1 :: 2 :: (foo 3 [3;3;4;5]) ->
       1 :: 2 :: [3; 4; 5] ->
       [1; 2; 3; 4; 5]

    *)

    let fooTail x lst =
        let rec aux c =
            function 
            | [] -> c []
            | y :: ys when x = y -> c ys
            | y :: ys -> aux (fun result -> c (y :: result)) ys

        aux id lst