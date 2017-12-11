open System.IO
open System

// ------------------------------------------------------------------------------
// ----------------------------------- Part 1 -----------------------------------
// ------------------------------------------------------------------------------

//Crossing the bridge, you've barely reached the other side of the stream when a program comes up to you, 
//clearly in distress. "It's my child process," she says, "he's gotten lost in an infinite grid!"
//Fortunately for her, you have plenty of experience with infinite grids.
//Unfortunately for you, it's a hex grid.
//The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the 
//north, northeast, southeast, south, southwest, and northwest:

//  \ n  /
//nw +--+ ne
//  /    \
//-+      +-
//  \    /
//sw +--+ se
//  / s  \

//You have the path the child process took. Starting where he started, you need to determine the fewest number of steps 
//required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)

//For example:

//ne,ne,ne is 3 steps away.
//ne,ne,sw,sw is 0 steps away (back where you started).
//ne,ne,s,s is 2 steps away (se,se).
//se,sw,se,sw,sw is 3 steps away (s,s,sw).

//Your puzzle answer was 687.
let part1 = 
    let input = (File.ReadAllText "input.txt").Split [|','|]
    let move step currentPosition = 
        let (x, y, z) = currentPosition
        match step with
        | step when step = "ne" -> (x, y - 1, z + 1)
        | step when step = "se" -> (x - 1, y, z + 1)
        | step when step = "s" -> (x - 1, y + 1, z)
        | step when step = "sw" -> (x, y + 1, z - 1)
        | step when step = "nw" -> (x + 1, y, z - 1)
        | _ -> (x + 1, y - 1, z)

    let getDistance currentPosition startPosition = 
        let (cx : int, cy : int, cz : int) = currentPosition
        let (sx : int, sy : int, sz : int) = startPosition
        (Math.Abs (cx - sx) + Math.Abs (cy - sy) + Math.Abs (cz - sz)) / 2

    let mutable distance = 0
    let startPosition = (0, 0, 0)
    let mutable currentPosition = startPosition

    for step in input do
        currentPosition <- move step currentPosition
        distance <- getDistance currentPosition startPosition

    printfn "Part1 result: %i" distance

part1

// ------------------------------------------------------------------------------
// ----------------------------------- Part 2 -----------------------------------
// ------------------------------------------------------------------------------

// How many steps away is the furthest he ever got from his starting position?
// Your puzzle answer was 1483.

let part2 = 
    let input = (File.ReadAllText "input.txt").Split [|','|]
    let move step currentPosition = 
        let (x, y, z) = currentPosition
        match step with
        | step when step = "ne" -> (x, y - 1, z + 1)
        | step when step = "se" -> (x - 1, y, z + 1)
        | step when step = "s" -> (x - 1, y + 1, z)
        | step when step = "sw" -> (x, y + 1, z - 1)
        | step when step = "nw" -> (x + 1, y, z - 1)
        | _ -> (x + 1, y - 1, z)

    let getDistance currentPosition startPosition = 
        let (cx : int, cy : int, cz : int) = currentPosition
        let (sx : int, sy : int, sz : int) = startPosition
        (Math.Abs (cx - sx) + Math.Abs (cy - sy) + Math.Abs (cz - sz)) / 2

    let mutable distance = 0
    let startPosition = (0, 0, 0)
    let mutable currentPosition = startPosition

    for step in input do
        currentPosition <- move step currentPosition
        let currentDistance = getDistance currentPosition startPosition
        if currentDistance > distance then
            distance <- currentDistance

    printfn "Part2 result: %i" distance

part2

System.Console.ReadKey() |> ignore