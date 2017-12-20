open System
open System.Collections.Generic
open System.IO
open System.Net.Mail

// ------------------------------------------------------------------------------
// ----------------------------------- Part 1 -----------------------------------
// ------------------------------------------------------------------------------

//You discover a tablet containing some strange assembly code labeled simply "Duet". 
//Rather than bother the sound card with it, you decide to run the code yourself. 
//Unfortunately, you don't see any documentation, so you're left to figure out what 
//the instructions mean on your own.

//It seems like the assembly is meant to operate on a set of registers that are each 
//named with a single letter and that can each hold a single integer. You suppose each 
//register should start with a value of 0.

//There aren't that many instructions, so it shouldn't be hard to figure out what 
//they do. Here's what you determine:

//snd X plays a sound with a frequency equal to the value of X.
//set X Y sets register X to the value of Y.
//add X Y increases register X by the value of Y.
//mul X Y sets register X to the result of multiplying the value contained in register 
//X by the value of Y.
//mod X Y sets register X to the remainder of dividing the value contained in register 
//X by the value of Y (that is, it sets X to the result of X modulo Y).
//rcv X recovers the frequency of the last sound played, but only when the value of X 
//is not zero. (If it is zero, the command does nothing.)
//jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater 
//than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the 
//previous instruction, and so on.)
//Many of the instructions can take either a register (a single letter) or a number. 
//The value of a register is the integer it contains; the value of a number is that number.

//After each jump instruction, the program continues with the instruction to which the jump 
//jumped. After any other instruction, the program continues with the next instruction. 
//Continuing (or jumping) off either end of the program terminates it.

//For example:

//set a 1
//add a 2
//mul a a
//mod a 5
//snd a
//set a 0
//rcv a
//jgz a -1
//set a 1
//jgz a -2
//The first four instructions set a to 1, add 2 to it, square it, and then set it to itself 
//modulo 5, resulting in a value of 4.
//Then, a sound with frequency 4 (the value of a) is played.
//After that, a is set to 0, causing the subsequent rcv and jgz instructions to both be 
//skipped (rcv because a is 0, and jgz because a is not greater than 0).
//Finally, a is set to 1, causing the next jgz instruction to activate, jumping back two 
//instructions to another jump, which jumps again to the rcv, which ultimately triggers 
//the recover operation.
//At the time the recover operation is executed, the frequency of the last sound played is 4.

//What is the value of the recovered frequency (the value of the most recently played sound) 
//the first time a rcv instruction is executed with a non-zero value?

//Your puzzle answer was 1187.

let getValue(register:string, registers:Dictionary<string, int64>) = 
    if registers.ContainsKey(register) then
        registers.[register]
    else
        registers.Add(register, (int64)0)
        (int64)0

let addOrSetValue(register:string, value:int64, registers:Dictionary<string, int64>) = 
    if registers.ContainsKey(register) then
        registers.[register] <- value
    else
        registers.Add(register, value)

let set(param1:string, param2:string, registers:Dictionary<string, int64>) = 
    if Int64.TryParse(param2) |> fst then
        let registerValue = Convert.ToInt64(param2)
        addOrSetValue(param1, registerValue, registers)
    else
        let registerValue = getValue(param2, registers)
        addOrSetValue(param1, registerValue, registers)

let add(param1:string, param2:string, registers:Dictionary<string, int64>) = 
    if Int64.TryParse(param2) |> fst then
        let registerValue = getValue(param1, registers)
        let valueToAdd = Convert.ToInt64(param2)
        addOrSetValue(param1, (registerValue + valueToAdd), registers)
    else
        let registerValue = getValue(param1, registers)
        let valueToAdd = getValue(param2, registers)
        addOrSetValue(param1, (registerValue + valueToAdd), registers)

let multiply(param1:string, param2:string, registers:Dictionary<string, int64>) = 
    if Int64.TryParse(param2) |> fst then
        let registerValue = getValue(param1, registers)
        let multiplier = Convert.ToInt64(param2)
        addOrSetValue(param1, (registerValue * multiplier), registers)
    else
        let registerValue = getValue(param1, registers)
        let multiplier = getValue(param2, registers)
        addOrSetValue(param1, (registerValue * multiplier), registers)

let modulo(param1:string, param2:string, registers:Dictionary<string, int64>) = 
    if Int64.TryParse(param2) |> fst then
        let registerValue = getValue(param1, registers)
        let divider = Convert.ToInt64(param2)
        addOrSetValue(param1, (registerValue % divider), registers)
    else
        let registerValue = getValue(param1, registers)
        let divider = getValue(param2, registers)
        addOrSetValue(param1, (registerValue % divider), registers)

let jump(param1:string, param2:string, registers:Dictionary<string, int64>) =
    let mutable registerValue = (int64)0
    if Int64.TryParse(param1) |> fst then
        registerValue <- Convert.ToInt64(param1)
    else
        registerValue <- getValue(param1, registers)
    if registerValue <= (int64)0 then
        (int64)0
    else if Int32.TryParse(param2) |> fst then
        Convert.ToInt64(param2)
    else
        registerValue

let part1 = 
    let send(param:string, registers:Dictionary<string, int64>, prevRegisters:Dictionary<string, int64>) = 
        let registerValue = getValue(param, registers)
        addOrSetValue(param, registerValue, prevRegisters)

    let recover(param:string, registers:Dictionary<string, int64>, prevRegisters:Dictionary<string, int64>) = 
        let registerValue = getValue(param, registers)
        if registerValue <> (int64)0 && prevRegisters.ContainsKey(param) then
            let prevRegisterValue = getValue(param, prevRegisters)
            addOrSetValue(param, prevRegisterValue, registers)
            true
        else
            false

    let instructions = File.ReadAllLines "input.txt" |> Array.map (fun e -> e.Split [|' '|])
    let registers = new Dictionary<string, int64>()
    let prevRegisters = new Dictionary<string, int64>()
    let mutable index = 0
    let mutable result = (int64)0
    let mutable endExecution = false

    while index < instructions.Length && endExecution <> true do
        let mutable jumpValue = 1
        if instructions.[index].[0] = "set" then
            set(instructions.[index].[1], instructions.[index].[2], registers)
        else if instructions.[index].[0] = "add" then
            add(instructions.[index].[1], instructions.[index].[2], registers)
        else if instructions.[index].[0] = "mul" then
            multiply(instructions.[index].[1], instructions.[index].[2], registers)
        else if instructions.[index].[0] = "mod" then
            modulo(instructions.[index].[1], instructions.[index].[2], registers)
        else if instructions.[index].[0] = "snd" then
            send(instructions.[index].[1], registers, prevRegisters)
        else if instructions.[index].[0] = "rcv" then
            if recover(instructions.[index].[1], registers, prevRegisters) then
                result <- getValue(instructions.[index].[1], registers)
                endExecution <- true
        else if instructions.[index].[0] = "jgz" then
            let tmp = (int32)(jump(instructions.[index].[1], instructions.[index].[2], registers))
            if tmp <> 0 then
                jumpValue <- tmp

        index <- index + jumpValue

    printfn "Part1 result: %i" result 
part1

// ------------------------------------------------------------------------------
// ----------------------------------- Part 2 -----------------------------------
// ------------------------------------------------------------------------------

//As you congratulate yourself for a job well done, you notice that the documentation 
//has been on the back of the tablet this entire time. While you actually got most of 
//the instructions correct, there are a few key differences. This assembly code isn't 
//about sound at all - it's meant to be run twice at the same time.

//Each running copy of the program has its own set of registers and follows the code 
//independently - in fact, the programs don't even necessarily run at the same speed. 
//To coordinate, they use the send (snd) and receive (rcv) instructions:

//snd X sends the value of X to the other program. These values wait in a queue until
//that program is ready to receive them. Each program has its own message queue, so a
//program can never receive a message it sent.
//rcv X receives the next value and stores it in register X. If no values are in the
//queue, the program waits for a value to be sent to it. Programs do not continue to 
//the next instruction until they have received a value. Values are received in the 
//order they are sent.
//Each program also has its own program ID (one 0 and the other 1); the register p 
//should begin with this value.

//For example:

//snd 1
//snd 2
//snd p
//rcv a
//rcv b
//rcv c
//rcv d
//Both programs begin by sending three values to the other. Program 0 sends 1, 2, 0; 
//program 1 sends 1, 2, 1. Then, each program receives a value (both 1) and stores it 
//in a, receives another value (both 2) and stores it in b, and then each receives the
//program ID of the other program (program 0 receives 1; program 1 receives 0) and 
//stores it in c. Each program now sees a different value in its own copy of register c.

//Finally, both programs try to rcv a fourth time, but no data is waiting for either 
//of them, and they reach a deadlock. When this happens, both programs terminate.

//It should be noted that it would be equally valid for the programs to run at 
//different speeds; for example, program 0 might have sent all three values and then 
//stopped at the first rcv before program 1 executed even its first instruction.

//Once both of your programs have terminated (regardless of what caused them to do so), 
//how many times did program 1 send a value?

//Your puzzle answer was 5969.

[<AllowNullLiteral>]
type Program (instructions : string[][], logName : string) =
    let _instructions = instructions
    let _logName = logName
    let mutable _parellProgram = null
    let _registers = new Dictionary<string, int64>()
    let mutable _currentStep = 0
    let mutable _queue = new Queue<int64>()
    let mutable _sendCount = 0

    member this.SetParellProgram(parellProgram : Program) =
        _parellProgram <- parellProgram

    member this.AddToQueue(value : int64) = 
        _queue.Enqueue(value)
    
    member this.Terminated =
        _currentStep >= _instructions.Length

    member this.SetRegister(register : string, value : int64) =
        set(register, value.ToString(), _registers)

    member this.Waiting =
        _queue.Count = 0

    member this.SendCount = 
        _sendCount

    member this.Execute =
        let mutable waiting = false

        while _currentStep < _instructions.Length && waiting <> true do
            let mutable jumpValue = 1
            if instructions.[_currentStep].[0] = "set" then
                set(instructions.[_currentStep].[1], instructions.[_currentStep].[2], _registers)
            else if instructions.[_currentStep].[0] = "add" then
                add(instructions.[_currentStep].[1], instructions.[_currentStep].[2], _registers)
            else if instructions.[_currentStep].[0] = "mul" then
                multiply(instructions.[_currentStep].[1], instructions.[_currentStep].[2], _registers)
            else if instructions.[_currentStep].[0] = "mod" then
                modulo(instructions.[_currentStep].[1], instructions.[_currentStep].[2], _registers)
            else if instructions.[_currentStep].[0] = "snd" then
                _parellProgram.AddToQueue(getValue(instructions.[_currentStep].[1], _registers))
                _sendCount <- _sendCount + 1
            else if instructions.[_currentStep].[0] = "rcv" then
                if _queue.Count <> 0 then
                    let register = _instructions.[_currentStep].[1]
                    addOrSetValue(register, _queue.Dequeue(), _registers)
                    jumpValue <- 1
                    waiting <- false
                else
                    jumpValue <- 0
                    waiting <- true
            else if instructions.[_currentStep].[0] = "jgz" then
                let tmp = (int32)(jump(instructions.[_currentStep].[1], instructions.[_currentStep].[2], _registers))
                if tmp <> 0 then
                    jumpValue <- tmp

            _currentStep <- _currentStep + jumpValue

let part2 = 
    let instructions = File.ReadAllLines "input.txt" |> Array.map (fun e -> e.Split [|' '|])
    let program1 = new Program(instructions, "p1.txt")
    let program2 = new Program(instructions, "p2.txt")

    program1.SetParellProgram(program2)
    program1.SetRegister("p", (int64)0)
    program2.SetParellProgram(program1)
    program2.SetRegister("p", (int64)1)

    program1.Execute
    program2.Execute

    while ((program1.Terminated || program2.Waiting) && (program1.Waiting || program2.Terminated)) <> true do
        program1.Execute
        program2.Execute

    printfn "Part2 result: %i" (program2.SendCount)

part2

Console.ReadKey() |> ignore