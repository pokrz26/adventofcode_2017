open System.Text
open System
open System.Collections.Generic

// ------------------------------------------------------------------------------
// ----------------------------------- Part 1 -----------------------------------
// ------------------------------------------------------------------------------

//Suddenly, a scheduled job activates the system's disk defragmenter. Were the situation different,
//you might sit and watch it for a while, but today, you just don't have that kind of time. It's 
//soaking up valuable system resources that are needed elsewhere, and so the only option is to 
//help it finish its task as soon as possible.

//The disk in question consists of a 128x128 grid; each square of the grid is either free or used.
//On this disk, the state of the grid is tracked by the bits in a sequence of knot hashes.

//A total of 128 knot hashes are calculated, each corresponding to a single row in the grid; each hash 
//contains 128 bits which correspond to individual grid squares. Each bit of a hash indicates whether
//that square is free (0) or used (1).

//The hash inputs are a key string (your puzzle input), a dash, and a number from 0 to 127 corresponding
//to the row. For example, if your key string were flqrgnkx, then the first row would be given by the bits
//of the knot hash of flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, 
//and so on until the last row, flqrgnkx-127.

//The output of a knot hash is traditionally represented by 32 hexadecimal digits; each of these digits
//correspond to 4 bits, for a total of 4 * 32 = 128 bits. To convert to bits, turn each hexadecimal 
//digit to its equivalent binary value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110,
//f becomes 1111, and so on; a hash that begins with a0c2017... in hexadecimal would begin with 
//10100000110000100000000101110000... in binary.

//Continuing this process, the first 8 rows and columns for key flqrgnkx appear as follows, using 
//# to denote used squares, and . to denote free ones:

//##.#.#..-->
//.#.#.#.#   
//....#.#.   
//#.#.##.#   
//.##.#...   
//##..#..#   
//.#...#..   
//##.#.##.-->
//|      |   
//V      V   
//In this example, 8108 squares are used across the entire 128x128 grid.

//Given your actual key string, how many squares are used?

//Your puzzle answer was 8148.

let hexToBinary hex =
    match hex with
    | hex when hex = '0' -> "0000"
    | hex when hex = '1' -> "0001"
    | hex when hex = '2' -> "0010"
    | hex when hex = '3' -> "0011"
    | hex when hex = '4' -> "0100"
    | hex when hex = '5' -> "0101"
    | hex when hex = '6' -> "0110"
    | hex when hex = '7' -> "0111"
    | hex when hex = '8' -> "1000"
    | hex when hex = '9' -> "1001"
    | hex when hex = 'A' -> "1010"
    | hex when hex = 'B' -> "1011"
    | hex when hex = 'C' -> "1100"
    | hex when hex = 'D' -> "1101"
    | hex when hex = 'E' -> "1110"
    | hex when hex = 'F' -> "1111"
    | _ -> raise (ArgumentException("This is not hex value."))

let getKnotHashInBinary text =
    let input = (text |> Seq.map (fun i -> (int32)i) |> Seq.toList) @ [17; 31; 73; 47; 23]
    let list = [|0 .. 255|]

    let listLength = Array.length list
    let mutable skipSize = 0
    let mutable index = 0

    for j = 0 to 63 do
        for length in input do
            let listPart = Array.rev [|for i in 0 .. (length - 1) -> list.[(index + i) % listLength] |]
            for i = 0 to length - 1 do
                list.[(index + i) % listLength] <- listPart.[i]
            index <- index + length + skipSize
            skipSize <- skipSize + 1

    let denseHash = [|for i in 0 .. 15 -> 0|]

    for i = 0 to 15 do
        denseHash.[i] <- list.[i * 16]
        for j = i * 16 + 1 to i * 16 + 15 do
            denseHash.[i] <- denseHash.[i] ^^^ list.[j]

    let stringBuilder = new StringBuilder()
    for elem in denseHash do
        stringBuilder.Append (elem.ToString("X2")) |> ignore

    let resultStringBuilder = new StringBuilder()
    for hex in stringBuilder.ToString() do
        resultStringBuilder.Append(hexToBinary hex) |> ignore

    resultStringBuilder.ToString()

let part1 =
    let input = "vbqugkhl"
    
    let rec getResult input index acc =
        match index with
        | index when index = 128 -> acc
        | _ -> getResult input (index + 1) (acc + ((getKnotHashInBinary (input + "-" + index.ToString())) |> Seq.filter (fun e -> e = '1') |> Seq.length))

    printfn "Part1 result: %i" (getResult input 0 0)

part1

// ------------------------------------------------------------------------------
// ----------------------------------- Part 1 -----------------------------------
// ------------------------------------------------------------------------------

//Now, all the defragmenter needs to know is the number of regions. A region is a group of used 
//squares that are all adjacent, not including diagonals. Every used square is in exactly one 
//region: lone used squares form their own isolated regions, while several adjacent squares 
//all count as a single region.

//In the example above, the following nine regions are visible, each marked with a distinct digit:

//11.2.3..-->
//.1.2.3.4   
//....5.6.   
//7.8.55.9   
//.88.5...   
//88..5..8   
//.8...8..   
//88.8.88.-->
//|      |   
//V      V   
//Of particular interest is the region marked 8; while it does not appear contiguous in this small 
//view, all of the squares marked 8 are connected when considering the whole 128x128 grid. 
//In total, in this example, 1242 regions are present.

//How many regions are present given your key string?

//Your puzzle answer was 1180.

let part2 =
    let tabSize = 127
    let input = [for i in 0..tabSize -> "vbqugkhl-" + i.ToString()]
    let matrix = [|for e in input -> getKnotHashInBinary e|] |> Array.map (fun e -> [|for c in e -> c|])
    let tmp = [|for i in 0..tabSize -> [|for i in 0..tabSize -> -1|]|]
    let mutable x = 0
    let mutable y = 0
    let mutable group = 0

    let a_zgwiazdka (group : int32, sx : int32, sy : int32, matrix : char[][], tab : int32[][]) =
        let elemntsToVisit = new Stack<Tuple<int, int>>()
        elemntsToVisit.Push((sx, sy))

        while elemntsToVisit.Count <> 0 do
            let (x, y) = elemntsToVisit.Pop()
            tab.[y].[x] <- group

            if x <> 0 && tab.[y].[x-1] = -1 && matrix.[y].[x-1] = '1' then 
                elemntsToVisit.Push((x-1, y)) 
            else if x <> 0 && tab.[y].[x-1] = -1 && matrix.[y].[x-1] = '0' then 
                tab.[y].[x-1] <- 0

            if y <> 0 && tab.[y-1].[x] = -1 && matrix.[y-1].[x] = '1' then 
                elemntsToVisit.Push((x, y-1)) 
            else if y <> 0 && tab.[y-1].[x] = -1 && matrix.[y-1].[x] = '0' then 
                tab.[y-1].[x] <- 0

            if x <> tabSize && tab.[y].[x+1] = -1 && matrix.[y].[x+1] = '1' then 
                elemntsToVisit.Push((x+1, y)) 
            else if x <> tabSize && tab.[y].[x+1] = -1 && matrix.[y].[x+1] = '0' then 
                tab.[y].[x+1] <- 0

            if y <> tabSize && tab.[y+1].[x] = -1 && matrix.[y+1].[x] = '1' then 
                elemntsToVisit.Push((x, y+1)) 
            else if y <> tabSize && tab.[y+1].[x] = -1 && matrix.[y+1].[x] = '0' then
                tab.[y+1].[x] <- 0

        ignore

    for row in matrix do
        x <- 0
        for _ in row do
            if tmp.[y].[x] = -1 && matrix.[y].[x] = '1' then 
                group <- group + 1
                a_zgwiazdka (group, x, y, matrix, tmp) |> ignore
            else if tmp.[y].[x] = -1 && matrix.[y].[x] = '0' then
                tmp.[y].[x] <- 0
            x <- x + 1
        y <- y + 1

    printfn "Part2 result: %i" group
part2

Console.ReadKey() |> ignore