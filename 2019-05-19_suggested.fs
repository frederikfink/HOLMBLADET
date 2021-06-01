module Question1
    type Peano =
    | O
    | S of Peano

//Q1.1
    let rec toInt =
        function
        | O -> 0u
        | S a -> 1u + toInt a

    let rec fromInt =
        function
        | 0u -> O
        | x -> S (fromInt (x - 1u))

//Q1.2
    let rec add a =
        function
        | O   -> a
        | S b -> S (add a b)

    let rec mult a =
        function
        | O   -> O
        | S b -> add a (mult a b)

    let rec pow a =
        function
        | O   -> S O
        | S b -> mult a (pow a b)       

//Q1.3
    let tailAdd a =
        let rec aux acc =
            function
            | O   -> acc
            | S b -> aux (S acc) b

        aux a

    let tailMult a =
        let rec aux acc =
            function
            | O   -> acc
            | S b -> aux (add a acc) b

        aux O

    let tailPow a =
        let rec aux acc =
            function
            | O   -> acc
            | S b -> aux (mult a acc) b

        aux (S O)

//Q1.4
    let rec loop f acc =
        function
        | O   -> acc
        | S a -> loop f (f acc) a

    let loopAdd a b = loop S a b
    let loopMult a b = loop (loopAdd a) O b
    let loopPow a b = loop (loopMult a) (S O) b

module Question2
    let rec f x =
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys when x <> y -> 
            match f x ys with
            | Some ys' -> Some (y::ys')
            | None     -> None

    let rec g xs =
        function
        | []    -> xs = []
        | y::ys -> 
            match f y xs with
            | Some xs' -> g xs' ys
            | None     -> false

    //Q 2.1

    (*
    f has type x:'a -> _arg1:'a list -> 'a list option when 'a : equality
    g has type xs:'a list -> _arg1:'a list -> bool when 'a : equality

    f x xs returns Some xs', where xs' is xs with the first element of x removed if
           x exists in xs and None otherwise

    g xs ys returns true if xs is a permutation of ys and false otherwise

    f can be called removeFirstOccurence
    g can be called isPermutation

    (Another name for g could be containsSameElements, or something along the lines. What is NOT a good name
     is anything regarding equality as the furction does NOT check equality.)

    *)

    //Q 2.2

    (* 
    The warning happens because F# does not realise that x = y and x <> y forms a complete pattern.

    (In the general case it is not possible to tell that a set of boolean cases is exhaustive and 
     therefor F# does not even try.)
    *)

    let rec f2 x =
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys -> 
            match f2 x ys with
            | Some ys' -> Some (y::ys')
            | None     -> None

    // Q 2.3

    let cons x xs = x :: xs

    let rec fOpt x = 
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys -> fOpt x ys |> Option.map (cons y)

    (* Alternative solution *)
    let rec fOpt2 x = 
        function
        | []                -> None
        | y::ys when x = y  -> Some ys
        | y::ys -> Option.map (fun ys' -> y :: ys') (fOpt2 x ys)

    let rec gOpt xs =
        function
        | []    -> xs = []
        | y::ys -> fOpt y xs |> Option.map (gOpt ys) |> Option.defaultValue false

    (* Alternative solution *)
    let rec gOpt2 xs =
        function
        | []    -> xs = []
        | y::ys -> Option.defaultValue false (Option.map (fun xs' -> gOpt2 xs' ys) (fOpt2 y xs))
        
    // Q 2.4

    (* 
    g is tail recursive

    Example

    g [1; 2; 3] [3; 2; 1] ->
    match f 3 [1; 2; 3] with
    | Some xs' -> g xs' [2; 1]
    | None     -> false ->
    g [1; 2] [2; 1] ->
    match f 2 [1; 2] with
    | Some xs' -> g xs' [1]
    | None     -> false ->
    g [1] [1] ->
    match f 1 [1] with
    | Some xs' -> g xs' []
    | None     -> false ->
    g [] [] ->
    true

    We can see that g itself is tail recursive (even though it calls f which is not) since all 
    recursive occurrences of g appear completely by themselves (in a tail recursive position)

    (The important thing for this assignment is that all expressions between the arrows evaluate
     to the exact same thing. If they do not then you are not demonstrating a proper reduction)
    *)

    (* Only implement the version of fTail or gTail that is not tail recursive *)

    let fTail x =
        let rec aux c =
            function 
            | [] -> c None
            | y :: ys when x = y -> c (Some ys)
            | y :: ys -> aux (Option.map (cons y) >> c) ys

        aux id


    (* Alternative solution *)
    let fTail2 x =
        let rec aux c =
            function 
            | [] -> None
            | y :: ys when x = y -> Some (c ys)
            | y :: ys -> aux (fun ys' -> c (y :: ys')) ys

        aux id

module Question3

    //Q 3.1

    let cpAux x = let x = decimal x * 2m in 4M / ((x * (x + 1m) * (x + 2m)))
    let rec nilakantha acc min =
        function
        | 0UL                    -> acc
        | x when min % 2UL = 1UL -> nilakantha (acc + cpAux min) (min + 1UL) (x - 1UL)
        | x                      -> nilakantha (acc - cpAux min) (min + 1UL) (x - 1UL)

    let calculatePi x = nilakantha 3M 1UL x

    //Q 3.2

    let piSeq = Seq.initInfinite (uint64 >> calculatePi)

    //Q 3.3

    let caAux r pi = pi * r * r
    let circleArea (radius : float) = 
        Seq.map (caAux (decimal radius)) piSeq

    let svAux r pi = pi * (4M / 3M) * r * r * r
    let sphereVolume (radius : float) =
        Seq.map (svAux (decimal radius)) piSeq

    //Q 3.4

    (* This solution is not very fast due to the recursive calls to tail, but it's fine for our purposes *)

    let circleSphere (radius : float) =
        let rec aux seq1 seq2 =
            seq {
                yield (Seq.head seq1, Seq.head seq2)
                yield! aux (Seq.tail seq1) (Seq.tail seq2)
            }

        aux (circleArea radius) (sphereVolume radius)


    (* Another possible, and much faster, solution *)
    let circleSphere2 (radius : float) = 
        let r = decimal radius
        seq { for pi in piSeq do yield (caAux r pi, svAux r pi) }

    //Q 3.5

    let parallelPi numberOfProcesses iterationsPerProcess = 
        [for i in 0UL..numberOfProcesses - 1UL do 
            yield async {return nilakantha 0M (i * iterationsPerProcess + 1UL) iterationsPerProcess}] |>
        Async.Parallel |>
        Async.RunSynchronously |>
        Array.sum |> 
        (+) 3M


        (* Timing *)

        (*

        calculatePi 1000000UL;;;
         Real: 00:00:00.186, CPU: 00:00:00.188, GC gen0: 0, gen1: 0
         val it : decimal = 3.1415926535897932382126441313M

        parallelPi 1UL 100000000UL;;
         Real: 00:00:17.663, CPU: 00:00:17.536, GC gen0: 0, gen1: 0
         val it : decimal = 3.1415926535964072965060128740M

        parallelPi 10UL 10000000UL;;
         Real: 00:00:05.559, CPU: 00:00:28.701, GC gen0: 0, gen1: 0
         val it : decimal = 3.1415926535964072965060128740M

        parallelPi 100UL 1000000UL;;
         Real: 00:00:04.425, CPU: 00:00:30.501, GC gen0: 1, gen1: 0
         val it : decimal = 3.1415926535964072965060128740M

        parallelPi 1000UL 100000UL;;
         Real: 00:00:04.547, CPU: 00:00:31.267, GC gen0: 0, gen1: 0
         val it : decimal = 3.1415926535964072965060128740M

        parallelPi 10000UL 10000UL;;
         Real: 00:00:04.745, CPU: 00:00:31.625, GC gen0: 1, gen1: 0
         val it : decimal = 3.1415926535964072965060128740M


        At 10000000UL we start seeing significant running times for single threaded processes, 
        and just calculating the same precision of pi but with ten processes cuts running times in 3.
        Increasing the processor count beyond that has little effect, however, and at 1000 proccesses the
        results start getting worse again.