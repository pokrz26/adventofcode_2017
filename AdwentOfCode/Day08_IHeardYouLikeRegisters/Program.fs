﻿open System.IO
open System.Collections.Generic
open System

// ------------------------------------------------------------------------------
// ----------------------------------- Part 1 -----------------------------------
// ------------------------------------------------------------------------------

//You receive a signal directly from the CPU. Because of your recent assistance with jump instructions, 
//it would like you to compute the result of a series of unusual register instructions.

//Each instruction consists of several parts: the register to modify, whether to increase or decrease that 
//register's value, the amount by which to increase or decrease it, and a condition. If the condition fails, 
//skip the instruction without modifying the register. The registers all start at 0. The instructions look like this:

//b inc 5 if a > 1
//a inc 1 if b < 5
//c dec -10 if a >= 1
//c inc -20 if c == 10
//These instructions would be processed as follows:

//Because a starts at 0, it is not greater than 1, and so b is not modified.
//a is increased by 1 (to 1) because b is less than 5 (it is 0).
//c is decreased by -10 (to 10) because a is now greater than or equal to 1 (it is 1).
//c is increased by -20 (to -10) because c is equal to 10.
//After this process, the largest value in any register is 1.

//You might also encounter <= (less than or equal to) or != (not equal to). However, the CPU doesn't have 
//the bandwidth to tell you what all the registers are named, and leaves that to you to determine.

//What is the largest value in any register after completing the instructions in your puzzle input?

//Your puzzle answer was 3612.
let part1 = 
    let input = File.ReadAllLines "input.txt" |> Seq.map (fun line -> line.Split [|' '|])
    let canChangeRegister a operation b =
        match operation with
        | operation when operation = ">" -> a > b
        | operation when operation = "<" -> a < b
        | operation when operation = ">=" -> a >= b
        | operation when operation = "<=" -> a <= b
        | operation when operation = "!=" -> a <> b
        | _ -> a = b
    let getRegisterValue register operation value =
        match operation with
        | operation when operation = "inc" -> register + value
        | _ -> register - value

    let registers = new Dictionary<string, int>()
    for line in input do
        if registers.ContainsKey line.[0] <> true then
            registers.[line.[0]] <- 0
        if registers.ContainsKey line.[4] <> true then
            registers.[line.[4]] <- 0

        if canChangeRegister registers.[line.[4]] line.[5] (Convert.ToInt32 line.[6]) then
            registers.[line.[0]] <- getRegisterValue registers.[line.[0]] line.[1] (Convert.ToInt32 line.[2])
            
    printfn "Part1 result: %i" (Seq.max registers.Values)

part1

// ------------------------------------------------------------------------------
// ----------------------------------- Part 1 -----------------------------------
// ------------------------------------------------------------------------------

//To be safe, the CPU also needs to know the highest value held in any register during this process 
//so that it can decide how much memory to allocate to these operations. For example, in the above 
//instructions, the highest value ever held was 10 (in register c after the third instruction was evaluated).

//Your puzzle answer was 3818.

let part2 = 
    let input = File.ReadAllLines "input.txt" |> Seq.map (fun line -> line.Split [|' '|])
    let canChangeRegister a operation b =
        match operation with
        | operation when operation = ">" -> a > b
        | operation when operation = "<" -> a < b
        | operation when operation = ">=" -> a >= b
        | operation when operation = "<=" -> a <= b
        | operation when operation = "!=" -> a <> b
        | _ -> a = b
    let getRegisterValue register operation value =
        match operation with
        | operation when operation = "inc" -> register + value
        | _ -> register - value

    let registers = new Dictionary<string, int>()
    let mutable highestValue = 0

    for line in input do
        if registers.ContainsKey line.[0] <> true then
            registers.[line.[0]] <- 0
        if registers.ContainsKey line.[4] <> true then
            registers.[line.[4]] <- 0

        if canChangeRegister registers.[line.[4]] line.[5] (Convert.ToInt32 line.[6]) then
            let newValue = getRegisterValue registers.[line.[0]] line.[1] (Convert.ToInt32 line.[2])
            registers.[line.[0]] <- newValue
            if newValue > highestValue then
                highestValue <- newValue
            
    printfn "Part2 result: %i" highestValue

part2

Console.ReadKey() |> ignore