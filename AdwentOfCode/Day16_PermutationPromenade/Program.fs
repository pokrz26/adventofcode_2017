open System
open System.IO
open System.Collections.Generic
open System.Text
open System.ComponentModel.Design

// ------------------------------------------------------------------------------
// ----------------------------------- Part 1 -----------------------------------
// ------------------------------------------------------------------------------

//You come upon a very unusual sight; a group of programs here appear to be dancing.

//There are sixteen programs in total, named a through p. They start by standing in a 
//line: a stands in position 0, b stands in position 1, and so on until p, which stands in position 15.

//The programs' dance consists of a sequence of dance moves:

//Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
//Exchange, written xA/B, makes the programs at positions A and B swap places.
//Partner, written pA/B, makes the programs named A and B swap places.
//For example, with only five programs standing in a line (abcde), they could do the following dance:

//s1, a spin of size 1: eabcd.
//x3/4, swapping the last two programs: eabdc.
//pe/b, swapping programs e and b: baedc.
//After finishing their dance, the programs end up in order baedc.

//You watch the dance for a while and record their dance moves (your puzzle input). In what order are the programs standing after their dance?

//Your puzzle answer was fgmobeaijhdpkcln.
let getDanceMove (elem:string) : Tuple<char, obj, obj> =
    match elem with
    | elem when elem.[0] = 's' -> ('s', box(Convert.ToInt32(elem.Substring(1, elem.Length - 1))), box(0))
    | elem when elem.[0] = 'x' -> 
        let tmp = elem.Substring(1, elem.Length - 1).Split [|'/'|]
        ('x', box(Convert.ToInt32(tmp.[0])), box(Convert.ToInt32(tmp.[1])))
    | elem when elem.[0] = 'p' ->
        let tmp = elem.Substring(1, elem.Length - 1).Split [|'/'|]
        ('p', box(tmp.[0].Trim().[0]), box(tmp.[1].Trim().[0]))
    |_ -> raise (ArgumentException())

let performSpinFigure(array:char[], spinsNumber:int) =
    let tmp = [|for e in array -> e|]
    let arrayLength = array.Length
    for i = 0 to array.Length - 1 do
        array.[(i + spinsNumber) % arrayLength] <- tmp.[i]

let performExchangeFigure(array:char[], p1:int, p2:int) =
    let tmp = array.[p1]
    array.[p1] <- array.[p2]
    array.[p2] <- tmp

let performPartnerFigure(array:char[], p1:char, p2:char) = 
    let p1Index = array |> Array.findIndex (fun e -> e = p1)
    let p2Index = array |> Array.findIndex (fun e -> e = p2)
    let tmp = array.[p1Index]
    array.[p1Index] <- array.[p2Index]
    array.[p2Index] <- tmp

let performDanceMove(array:char[], danceMove: Tuple<char, obj, obj>) =
    let (danceFigure, param1, param2) = danceMove

    match danceFigure with
    | danceFigure when danceFigure = 's' -> performSpinFigure(array, unbox<int> param1)
    | danceFigure when danceFigure = 'x' -> performExchangeFigure(array, unbox<int> param1, unbox<int> param2)
    | danceFigure when danceFigure = 'p' -> performPartnerFigure(array, unbox<char> param1, unbox<char> param2)
    | _ -> raise (ArgumentException())

    ignore

let part1 = 
    //let input2 = "s1,x3/4,pe/b".Split [|','|]
    let input = (File.ReadAllText "input.txt").Split [|','|]
    let danceMoves = input |> Array.map (fun e -> getDanceMove(e))
    //let array2 = [|for i in 0..4 -> (char)((int)'a' + i)|]
    let array = [|for i in 0..15 -> (char)((int)'a' + i)|]

    for danceMove in danceMoves do
        performDanceMove(array, danceMove) |> ignore

    printfn "Part1 result:"
    array |> Array.iter (fun e -> printf "%c" e)
    printfn ""

part1

// ------------------------------------------------------------------------------
// ----------------------------------- Part 2 -----------------------------------
// ------------------------------------------------------------------------------

//Now that you're starting to get a feel for the dance moves, you turn your attention to the 
//dance as a whole.

//Keeping the positions they ended up in from their previous dance, the programs perform it 
//again and again: including the first dance, a total of one billion (1000000000) times.

//In the example above, their second dance would begin with the order baedc, and use the same dance moves:

//s1, a spin of size 1: cbaed.
//x3/4, swapping the last two programs: cbade.
//pe/b, swapping programs e and b: ceadb.
//In what order are the programs standing after their billion dances?

let part2 = 
    //let input = "s1,x3/4,pe/b".Split [|','|]
    let input = (File.ReadAllText "input.txt").Split [|','|]
    let danceMoves = input |> Array.map (fun e -> getDanceMove(e))
    //let array = [|for i in 0..4 -> (char)((int)'a' + i)|]
    let array = [|for i in 0..15 -> (char)((int)'a' + i)|]
    let history = new Dictionary<string, int>()
    let mutable hasResult = false;
    let mutable index = 0
    let max = 1_000_000_000
    let mutable result = ""

    let getDanceString (array : char[]) = 
        let result = new StringBuilder()
        for c in array do
            result.Append(c) |> ignore
        result.ToString()

    while index < 1_000_000_000 && hasResult <> true do
        for danceMove in danceMoves do
            performDanceMove(array, danceMove) |> ignore
        
        let dance = getDanceString(array)

        if history.ContainsKey(dance) then
            hasResult <- true
            let prevResultIndex = history.[dance]
            let delta = index - prevResultIndex
            let rest = max % delta
            result <- (string)(history |> Seq.filter (fun e -> e.Value = (prevResultIndex + rest - 1)) |> Seq.toArray).[0].Key
        else
            history.Add(dance, index)
        index <- index + 1

    printfn "Part2 result: %s" result

part2

Console.ReadKey() |> ignore