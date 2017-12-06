open System

// ----------------------------------------------------------------------------
// ---------------------------------- Part 1 ----------------------------------
// ----------------------------------------------------------------------------

//You come across an experimental new kind of memory stored on an infinite two-dimensional grid.
//Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then
//counting up while spiraling outward. For example, the first few squares are allocated like this:
//17  16  15  14  13
//18   5   4   3  12
//19   6   1   2  11
//20   7   8   9  10
//21  22  23---> ...
//While this is very space-efficient (no squares are skipped), requested data must be carried back to 
//square 1 (the location of the only access port for this memory system) by programs that can only 
//move up, down, left, or right. They always take the shortest path: the Manhattan Distance between 
//the location of the data and square 1.
//For example:
//Data from square 1 is carried 0 steps, since it's at the access port.
//Data from square 12 is carried 3 steps, such as: down, left, left.
//Data from square 23 is carried only 2 steps: up twice.
//Data from square 1024 must be carried 31 steps.
//How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?
//Your puzzle input is still 361527.

// Your puzzle answer was 326.
let getStepsToCarryDataToAccessPort value = 
    if value = 1 then
        0
    else
        let mutable spiralNumber = 1
        let mutable spiralStartPointValue = 2
        let mutable an = 8
        let mutable prevDelta = value - spiralStartPointValue
        let mutable delta = value - spiralStartPointValue - an - 1

        // ------------------------------- detect on which spiral is input value --------------------------------------
        // if next spiral startpoint is greeter than input and current spiral start point is lower than input then
        // we are on the next spiral and we don't need to use loop implemented below.
        if delta < 0 && prevDelta >= 0 then
            spiralStartPointValue <- spiralStartPointValue + an + 1
            an <- an + 8
            spiralNumber <- spiralNumber + 1
            prevDelta <- delta
            delta <- value - spiralStartPointValue - an - 1

        while (delta > 0 && delta <= prevDelta) do
            spiralStartPointValue <- spiralStartPointValue + an + 1
            an <- an + 8
            spiralNumber <- spiralNumber + 1
            prevDelta <- delta
            delta <- value - spiralStartPointValue - an - 1

        // ----------------- detect on which wall on spiral is input value (top, left, right, bottom) -----------------
        let distanceToNextStartPointOnSpiral = 2 + (spiralNumber - 1) * 2
        prevDelta <- Math.Abs(value - spiralStartPointValue)
        delta <- Math.Abs(value - spiralStartPointValue - distanceToNextStartPointOnSpiral)

        while delta < prevDelta do
            spiralStartPointValue <- spiralStartPointValue + distanceToNextStartPointOnSpiral
            prevDelta <- delta
            delta <- Math.Abs(value - spiralStartPointValue - distanceToNextStartPointOnSpiral)

        // Get distance to 1
        let distanceToOne = spiralNumber + Math.Abs(value - spiralStartPointValue)

        distanceToOne

let part1 = 
    let input = 361527
    printfn "part 1 result: %i" (getStepsToCarryDataToAccessPort input)

part1

// ----------------------------------------------------------------------------
// ---------------------------------- Part 2 ----------------------------------
// ----------------------------------------------------------------------------

//As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, 
//in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.
//So, the first few squares' values are chosen as follows:
//Square 1 starts with the value 1.
//Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
//Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
//Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
//Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
//Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:
//147  142  133  122   59
//304    5    4    2   57
//330   10    1    1   54
//351   11   23   25   26
//362  747  806--->   ...
//What is the first value written that is larger than your puzzle input?
//Your puzzle input is still 361527.

//Your puzzle answer was 363010.
let getElementValue i j prevSpiral currentSpiral acs = 
    let prevSpiralLength = Array.length prevSpiral
    let currentSpiralLength = Array.length currentSpiral
    match i with
    | i when i = 0                          -> prevSpiral.[j % prevSpiralLength] + prevSpiral.[(j + 1) % prevSpiralLength]
    | i when i = currentSpiralLength - 1    -> prevSpiral.[j % prevSpiralLength] + currentSpiral.[(i - 1) % currentSpiralLength] + currentSpiral.[(i + 1) % currentSpiralLength]
    | i when i = currentSpiralLength - 2    -> prevSpiral.[(j - 1) % prevSpiralLength] + prevSpiral.[j % prevSpiralLength] + currentSpiral.[(i - 1) % currentSpiralLength] + currentSpiral.[(i + 2) % currentSpiralLength]
    | i when (i + 1) % acs = 0              -> prevSpiral.[j % prevSpiralLength] + currentSpiral.[(i - 1) % currentSpiralLength]
    | i when (i + 2) % acs = 0              -> prevSpiral.[(j - 1) % prevSpiralLength] + prevSpiral.[j % prevSpiralLength] + currentSpiral.[(i - 1) % currentSpiralLength]
    | i when i % acs = 0                    -> prevSpiral.[j % prevSpiralLength] + prevSpiral.[(j + 1) % prevSpiralLength] + currentSpiral.[(i - 1) % currentSpiralLength] + currentSpiral.[(i - 2) % currentSpiralLength]
    | _                                     -> prevSpiral.[(j - 1) % prevSpiralLength] + prevSpiral.[j % prevSpiralLength] + prevSpiral.[(j + 1) % prevSpiralLength] + currentSpiral.[(i - 1) % currentSpiralLength]

let part2 = 
    let input = 361527
    let mutable aps = 2                                                 // previous spiral wall height
    let mutable acs = aps + 2                                           // current spiral wall height
    let mutable prevSpiral = [|1; 2; 4; 5; 10; 11; 23; 25|]
    let mutable currentSpiral = Array.zeroCreate (acs * 4)
    let prevSpiralLength = Array.length prevSpiral - 1
    let mutable currentSpiralLength = Array.length currentSpiral - 1
    let mutable i = 0                                                   // current spiral index
    let mutable j = prevSpiralLength                                    // previous spiral index
    let mutable currentElementValue = prevSpiral.[prevSpiralLength]

    while input > currentElementValue do
        if i > currentSpiralLength then
            aps <- acs
            acs <- acs + 2
            prevSpiral <- currentSpiral
            currentSpiral <- Array.zeroCreate (acs * 4)
            currentSpiralLength <- Array.length currentSpiral - 1
            i <- 0
            j <- Array.length prevSpiral - 1
        currentElementValue <- getElementValue i j prevSpiral currentSpiral acs
        currentSpiral.[i] <- currentElementValue
        // Do not increment j if we are on corner or close to corner
        if (i + 2) % acs <> 0 && (i + 1) % acs <> 0 && i - 1 <> currentSpiralLength && i <> currentSpiralLength then
            j <- j + 1
        i <- i + 1

    printfn "part 2 result = %i" currentElementValue

part2

System.Console.ReadKey() |> ignore