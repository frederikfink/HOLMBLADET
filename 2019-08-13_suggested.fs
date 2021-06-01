module Question1

module Question2
    let f s =
        let l = String.length s
        let rec aux =
            function
            | i when i = l -> []
            | i -> s.[i] :: aux (i + 1)

        aux 0

    let g s = 
        s |> f |>
        List.filter System.Char.IsLetter |>
        List.map System.Char.ToLower |>
        fun lst -> lst = List.rev lst

    //Q 2.1

    (*
    f has type string -> char list
    g has type string -> bool

    f takes a string s and returns a list with all characters in s and with the order preserved
    g takes a string s and returns true if s is a palindrome and false othrewise. The function ignores
    characters that are not letters and is case insensitive.

    An appropriate name for f would be explode or stringToList
    An appropriate name for g would be isPalindrome
    *)

    //Q 2.2


    let f2 s = [for c in s do yield c]

    //Q 2.3

    let g2 = 
        f >>
        List.filter System.Char.IsLetter >>
        List.map System.Char.ToLower >>
        fun lst -> lst = List.rev lst

    //Q 2.4

    (* 

    The inner function aux of f is not tail recursive as the following derivation shows.

    f "ITU" -->
    let l = 3
    let rec aux =
        function
        | i when i = 3 -> []
        | i -> "ITU".[i] :: aux (i + 1)

    aux 0 -->

    "ITU".[0] :: aux (0 + 1) -->
    'I' :: aux 1 -->

    'I' :: "ITU".[1] :: aux (1 + 1) -->
    'I' :: 'T' :: aux 2 -->

    'I' :: 'T' :: "ITU".[2] :: aux (2 + 1) -->
    'I' :: 'T' :: 'U' :: aux 3 -->

    'I' :: 'T' :: 'U' :: [] -->
    ['I'; 'T'; 'U']

    The computation produces a long list of cons-operators that cannot be resolved until the end of the string
    has been reached.

    *)

    let fTail s =
        let l = String.length s
        let rec aux c =
            function 
            | i when i = l -> c []
            | i -> aux (fun result -> c (s.[i]::result)) (i + 1)

        aux id 0
    //Q 2.5

    let gOpt (s : string) =
        let rec aux i =
            function 
            | j when i >= j -> true
            | j when not (System.Char.IsLetter s.[i]) -> 
                 aux (i + 1) j
            | j when not (System.Char.IsLetter s.[j]) ->
                aux i (j - 1)
            | j when System.Char.ToLower s.[i] = System.Char.ToLower s.[j] ->
                aux (i + 1) (j - 1)
            | _ -> false

        aux 0 (String.length s - 1)

module Question3
     //Q 3.1

    let calculateGoldenRatio x =
        let rec aux a b =
            function
            | 0 -> float b / float a
            | n -> aux b (a + b) (n - 1)

        aux 1 1 x
        
    //Q 3.2

    let grSeq = Seq.unfold (fun (a, b) -> Some (b / a, (b, a + b))) (1.0, 1.0)

    //Q 3.3

    let goldenRectangleSeq x = 
        Seq.map (fun i -> x * x * i) grSeq

    let goldenTriangleSeq b = 
        let height i = b * System.Math.Sqrt (i * i - 1.0 / 4.0)
        Seq.map (fun x -> b * height x / 2.0) grSeq

    //Q 3.4

    let goldenRectangleTriangle b =
        let rec aux s1 s2 =
            seq {
                yield (Seq.head s1, Seq.head s2)
                yield! aux (Seq.tail s1) (Seq.tail s2)
            }

        aux (goldenRectangleSeq b) (goldenTriangleSeq b)
