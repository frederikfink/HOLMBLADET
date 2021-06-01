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