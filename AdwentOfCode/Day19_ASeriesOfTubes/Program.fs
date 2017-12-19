﻿open System
open System.IO
open System.Text

// ------------------------------------------------------------------------------
// ----------------------------------- Part 1 -----------------------------------
// ------------------------------------------------------------------------------

//Somehow, a network packet got lost and ended up here. It's trying to follow a 
//routing diagram (your puzzle input), but it's confused about where to go.

//Its starting point is just off the top of the diagram. Lines (drawn with |, -, and +) 
//show the path it needs to take, starting by going down onto the only line connected 
//to the top of the diagram. It needs to follow this path until it reaches the end 
//(located somewhere within the diagram) and stop there.

//Sometimes, the lines cross over each other; in these cases, it needs to continue 
//going the same direction, and only turn left or right when there's no other option. 
//In addition, someone has left letters on the line; these also don't change its 
//direction, but it can use them to keep track of where it's been. For example:

//     |          
//     |  +--+    
//     A  |  C    
// F---|----E|--+ 
//     |  |  |  D 
//     +B-+  +--+ 

//Given this diagram, the packet needs to take the following path:

//Starting at the only line touching the top of the diagram, it must go down, pass
//through A, and continue onward to the first +.
//Travel right, up, and right, passing through B in the process.
//Continue down (collecting C), right, and up (collecting D).
//Finally, go all the way left through E and stopping at F.
//Following the path to the end, the letters it sees on its path are ABCDEF.

//The little packet looks up at you, hoping you can help it find the way. What 
//letters will it see (in the order it would see them) if it follows the path? 
//(The routing diagram is very wide; make sure you view it without line wrapping.)

//Your puzzle answer was VTWBPYAQFU.

let part1 = 
    let maze = File.ReadAllLines "input.txt" |> Array.map (fun e -> [|for c in e -> c|])
    let mutable y = 0
    let mutable x = maze.[0] |> Array.findIndex (fun e -> e = '|')
    let mutable x_direction = 0
    let mutable y_direction = 1
    let result = new StringBuilder()

    while maze.[y].[x] <> ' ' do
        if maze.[y].[x] = '+' then
            if x_direction <> 0 then
                x_direction <- 0
                if y + 1 < maze.Length && maze.[y + 1].[x] <> ' ' && maze.[y + 1].[x] <> '+' then
                    y_direction <- 1
                else
                    y_direction <- -1
            else
                y_direction <- 0
                if x + 1 < maze.[0].Length && maze.[y].[x + 1] <> ' ' && maze.[y].[x + 1] <> '+' then
                    x_direction <- 1
                else
                    x_direction <- -1
        if maze.[y].[x] <> ' ' && maze.[y].[x] <> '+' && maze.[y].[x] <> '|' && maze.[y].[x] <> '-' then
            result.Append(maze.[y].[x]) |> ignore
        x <- x + x_direction
        y <- y + y_direction

    printfn "Part1 result: %s" (result.ToString())

part1

// ------------------------------------------------------------------------------
// ----------------------------------- Part 2 -----------------------------------
// ------------------------------------------------------------------------------

//The packet is curious how many steps it needs to go.

//For example, using the same routing diagram from the example above...

//     |          
//     |  +--+    
//     A  |  C    
// F---|--|-E---+ 
//     |  |  |  D 
//     +B-+  +--+ 

//...the packet would go:

//6 steps down (including the first line at the top of the diagram).
//3 steps right.
//4 steps up.
//3 steps right.
//4 steps down.
//3 steps right.
//2 steps up.
//13 steps left (including the F it stops on).
//This would result in a total of 38 steps.

//How many steps does the packet need to go?

//Although it hasn't changed, you can still get your puzzle input.

//Your puzzle answer was 17358.

let part2 = 
    let maze = File.ReadAllLines "input.txt" |> Array.map (fun e -> [|for c in e -> c|])
    let mutable y = 0
    let mutable x = maze.[0] |> Array.findIndex (fun e -> e = '|')
    let mutable x_direction = 0
    let mutable y_direction = 1
    let mutable steps = 0

    while maze.[y].[x] <> ' ' do
        if maze.[y].[x] = '+' then
            if x_direction <> 0 then
                x_direction <- 0
                if y + 1 < maze.Length && maze.[y + 1].[x] <> ' ' && maze.[y + 1].[x] <> '+' then
                    y_direction <- 1
                else
                    y_direction <- -1
            else
                y_direction <- 0
                if x + 1 < maze.[0].Length && maze.[y].[x + 1] <> ' ' && maze.[y].[x + 1] <> '+' then
                    x_direction <- 1
                else
                    x_direction <- -1
        steps <- steps + 1
        x <- x + x_direction
        y <- y + y_direction

    printfn "Part2 result: %i" steps

part2

Console.ReadKey() |> ignore