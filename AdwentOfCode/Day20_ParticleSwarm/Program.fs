open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

// ------------------------------------------------------------------------------
// ----------------------------------- Part 1 -----------------------------------
// ------------------------------------------------------------------------------

//Suddenly, the GPU contacts you, asking for help. Someone has asked it to simulate 
//too many particles, and it won't be able to finish them all in time to render the 
//next frame at this rate.

//It transmits to you a buffer (your puzzle input) listing each particle in order 
//(starting with particle 0, then particle 1, particle 2, and so on). For each 
//particle, it provides the X, Y, and Z coordinates for the particle's position 
//(p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

//Each tick, all particles are updated simultaneously. A particle's properties are 
//updated in the following order:

//Increase the X velocity by the X acceleration.
//Increase the Y velocity by the Y acceleration.
//Increase the Z velocity by the Z acceleration.
//Increase the X position by the X velocity.
//Increase the Y position by the Y velocity.
//Increase the Z position by the Z velocity.
//Because of seemingly tenuous rationale involving z-buffering, the GPU would like to 
//know which particle will stay closest to position <0,0,0> in the long term. Measure 
//this using the Manhattan distance, which in this situation is simply the sum of the 
//absolute values of a particle's X, Y, and Z position.

//For example, suppose you are only given two particles, both of which stay entirely 
//on the X-axis (for simplicity). Drawing the current states of particles 0 and 1 
//(in that order) with an adjacent a number line and diagram of current X positions
//(marked in parenthesis), the following would take place:

//p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
//p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

//p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
//p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

//p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
//p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

//p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
//p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)   
//At this point, particle 1 will never be closer to <0,0,0> than particle 0, and so, in
//the long run, particle 0 will stay closest.

//Which particle will stay closest to position <0,0,0> in the long term?

//Your puzzle answer was 144.

let parseGroup(input : string) : Tuple<int, int, int> = 
    let numbers = input.Trim().Substring(1, input.Length - 2).Split [|','|]
    (Convert.ToInt32(numbers.[0]), Convert.ToInt32(numbers.[1]), Convert.ToInt32(numbers.[2]))

let getManhatanDistance(point:Tuple<int, int, int>) : int =
    let (x, y, z) = point
    Math.Abs(x) + Math.Abs(y) + Math.Abs(z)

let part1 = 
    let input = File.ReadAllLines "input.txt"
    let points = [|for i in 0..input.Length - 1 -> (0, 0, 0)|]
    let velocities = [|for i in 0..input.Length - 1 -> (0, 0, 0)|]
    let accelerations = [|for i in 0..input.Length - 1 -> (0, 0, 0)|]

    for i = 0 to input.Length - 1 do
        let tmp = Regex.Matches(input.[i], @"<[^>]*>")
        let enumerator = tmp.GetEnumerator()
        enumerator.MoveNext() |> ignore
        points.[i] <- parseGroup((string)enumerator.Current)
        enumerator.MoveNext() |> ignore
        velocities.[i] <- parseGroup((string)enumerator.Current)
        enumerator.MoveNext() |> ignore
        accelerations.[i] <- parseGroup((string)enumerator.Current)

    let mutable closestPoint = points.Length
    let mutable closestPoint_PointDistance = Int32.MaxValue
    let mutable closestPoint_VelocityDistance = Int32.MaxValue
    let mutable closestPoint_AccelerationDistance = Int32.MaxValue

    for i = 0 to points.Length - 1 do
        let pDistance = getManhatanDistance(points.[i])
        let vDistance = getManhatanDistance(velocities.[i])
        let aDistance = getManhatanDistance(accelerations.[i])

        if aDistance < closestPoint_AccelerationDistance
            || aDistance = closestPoint_AccelerationDistance && vDistance < closestPoint_VelocityDistance
            || aDistance = closestPoint_AccelerationDistance && vDistance = closestPoint_VelocityDistance && pDistance < closestPoint_PointDistance then
            closestPoint <- i
            closestPoint_PointDistance <- pDistance
            closestPoint_VelocityDistance <- vDistance
            closestPoint_AccelerationDistance <- aDistance

    printfn "Part1 result: %i" closestPoint

part1

// ------------------------------------------------------------------------------
// ----------------------------------- Part 2 -----------------------------------
// ------------------------------------------------------------------------------

//To simplify the problem further, the GPU would like to remove any particles that collide. Particles 
//collide if their positions ever exactly match. Because particles are updated simultaneously, more 
//than two particles can collide at the same time and place. Once particles collide, they are removed 
//and cannot collide with anything else after that tick.

//For example:

//p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>    
//p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
//p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
//p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>

//p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>    
//p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
//p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)   
//p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>

//p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>    
//p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
//p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)      
//p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>

//------destroyed by collision------    
//------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
//------destroyed by collision------                      (3)         
//p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>
//In this example, particles 0, 1, and 2 are simultaneously destroyed at the time and place marked X. 
//On the next tick, particle 3 passes through unharmed.

//How many particles are left after all collisions are resolved?

//Your puzzle answer was 477.

type P (point : Tuple<int, int, int>, velocity : Tuple<int, int, int>, acceleration : Tuple<int, int, int>) =
    let mutable _point = point
    let mutable _velocity = velocity
    let _acceleration = acceleration

    member this.Point =
        _point

    member this.DoStep = 
        let (px, py, pz) = _point
        let (vx, vy, vz) = _velocity
        let (ax, ay, az) = _acceleration
        let (nvx, nvy, nvz) = (vx + ax, vy + ay, vz + az)
        let (npx, npy, npz) = (px + nvx, py + nvy, pz + nvz)
        _velocity <- (nvx, nvy, nvz)
        _point <- (npx, npy, npz)

    member this.Collide(point:P) =
        let (x1, y1, z1) = _point
        let (x2, y2, z2) = point.Point
        x1 = x2 && y1 = y2 && z1 = z2

let part2 = 
    let input = File.ReadAllLines "input.txt"
    let points = new List<P>()

    for i = 0 to input.Length - 1 do
        let tmp = Regex.Matches(input.[i], @"<[^>]*>")
        let enumerator = tmp.GetEnumerator()
        enumerator.MoveNext() |> ignore
        let point = parseGroup((string)enumerator.Current)
        enumerator.MoveNext() |> ignore
        let velocity = parseGroup((string)enumerator.Current)
        enumerator.MoveNext() |> ignore
        let acceleration = parseGroup((string)enumerator.Current)
        points.Add(new P(point, velocity, acceleration))

    let mutable notCollidingPoints = [for p in points -> p]
    let mutable repeat = 1000
    while repeat > 0 do
        repeat <- repeat - 1
        let mutable tmp = [for p in notCollidingPoints -> p]
        for p in notCollidingPoints do
            tmp <- tmp |> List.filter (fun e -> (e <> p && e.Collide(p)) <> true)
        notCollidingPoints <- tmp

        for p in notCollidingPoints do
            p.DoStep

    printfn "Part2 result: %i" (notCollidingPoints.Length)

part2

Console.ReadKey() |> ignore