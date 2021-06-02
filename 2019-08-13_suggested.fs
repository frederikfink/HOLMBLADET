module Question1
    
type Sum<'A, 'B> =
    | Left of 'A
    | Right of 'B

(* Question 1.1 *)

let sum1 = Left [5]
let sum2 = Right (Some true)

let sumMap f g =
    function 
    | Left a -> f a
    | Right b -> g b

(* Question 1.2 *)

type SumColl<'A, 'B> =
| Nil
| CLeft of 'A * SumColl<'A, 'B>
| CRight of 'B * SumColl<'A, 'B>

let sumColl = CLeft ([true], CRight (42, Nil))

let rec ofList =
    function
    | [] -> Nil
    | Left a :: ss -> CLeft (a, ofList ss)
    | Right b :: ss -> CRight (b, ofList ss)

(* Question 1.3 *)

let reverse lst =
    let rec aux acc =
        function
        | Nil -> acc
        | CLeft (a, ss) -> aux (CLeft (a, acc)) ss
        | CRight (b, ss) -> aux (CRight (b, acc)) ss

    aux Nil lst

(* Question 1.4 *)

let lcons ss a = CLeft(a, ss)
let rcons ss b = CRight(b, ss)

let ofList2 lst =
    List.foldBack (fun x acc -> sumMap (lcons acc) (rcons acc) x) lst Nil

(* Question 1.5 *)

let rec foldBackSumColl f g =
    function
    | Nil -> id
    | CLeft (a, ss)  -> f a << foldBackSumColl f g ss
    | CRight (b, ss) -> g b << foldBackSumColl f g ss

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


module Question4Solution

    (* Question 4.1 *)

    type colour = Red | Green | Blue | Purple | Orange | Yellow
    type shape  = Square | Circle | Star | Diamond | Club | Cross

    type tile = Tile of colour * shape

    let mkColour =
        function
        | "red"    -> Red
        | "green"  -> Green
        | "blue"   -> Blue
        | "purple" -> Purple
        | "orange" -> Orange
        | "yellow" -> Yellow
        | _        -> failwith "Invalid colour"

    let mkShape =
        function
        | "square"  -> Square
        | "circle"  -> Circle
        | "star"    -> Star
        | "diamond" -> Diamond
        | "club"    -> Club
        | "cross"   -> Cross
        | _        -> failwith "Invalid shape"

    let mkTile c s = Tile(mkColour c, mkShape s)

    let colourToString =
        function
        | Red    -> "red"
        | Green  -> "green"
        | Blue   -> "blue"
        | Purple -> "purple"
        | Orange -> "orange"
        | Yellow -> "yellow"

    let shapeToString =
        function
        | Square  -> "square"
        | Circle  -> "circle"
        | Star    -> "star"
        | Diamond -> "diamond"
        | Club    -> "club"
        | Cross   -> "cross"

    let tileToString (Tile(c, s)) = 
        sprintf "%s %s" (colourToString c) (shapeToString s)

    (* Question 4.2 *)

    let validTiles ts (Tile (c, s)) =
        List.forall (fun (Tile (c', s')) -> c = c' && s <> s') ts || 
        List.forall (fun (Tile (c', s')) -> c <> c' && s = s') ts

    (* Question 4.3 optional *)

    type coord = Coord of int * int
    type board = Board of Map<coord, tile>
    type direction = Left | Right | Up | Down

    let moveCoord (Coord (x, y)) =
        function
        | Left  -> Coord (x - 1, y)
        | Right -> Coord (x + 1, y)
        | Up    -> Coord (x, y - 1)
        | Down  -> Coord (x, y + 1)

    let rec collectTiles (Board b) c d =
        match Map.tryFind c b with
        | Some t -> t :: collectTiles (Board b) (moveCoord c d) d
        | None -> []

    (* Question 4.4 *)
    
    let placeTile (coord, tile) (Board b as board) =
        let f d = collectTiles board (moveCoord coord d) d
        if Option.isNone (Map.tryFind coord b) && 
           validTiles (f Left @ f Right) tile && 
           validTiles (f Up @ f Down) tile then
            Some (Board (Map.add coord tile b))
        else
            None

    (* Question 4.5 *)

    (* You may use *either* railroad-oriented programming or computation expressions.
       You do not have to use both *)

    (* Railroad-oriented programming *)
    let ret = Some
    let bind f =
        function
        | None   -> None
        | Some x -> f x
    let (>>=) x f = bind f x

    (* Computation expressions *)
    type opt<'a> = 'a option
    type OptBuilderClass() =
        member t.Return (x : 'a) : opt<'a> = ret x
        member t.ReturnFrom (x : opt<'a>) = x
        member t.Bind (o : opt<'a>, f : 'a -> opt<'b>) : opt<'b> = bind f o
    let opt = new OptBuilderClass()

    let placeTiles coords board = List.foldBack (bind << placeTile) coords (ret board)

    // Alternative solutions

    let rec placeTiles2 coords board =
        match coords with
        | [] -> ret board
        | ct::cts -> placeTile ct board >>= fun b -> placeTiles2 cts b

    let rec placeTiles3 coords board =
        opt {
            match coords with
            | [] -> return board
            | ct :: cts -> 
                let! newBoard = placeTile ct board
                return! placeTiles3 cts newBoard
        }



