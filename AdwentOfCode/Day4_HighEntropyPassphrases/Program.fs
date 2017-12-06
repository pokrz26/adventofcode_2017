open System.IO
open System

let input = File.ReadLines "input.txt"

// -------------------------------------------------------------------------------
// ------------------------------- Part 1 ----------------------------------------
// -------------------------------------------------------------------------------

//A new system policy has been put in place that requires all accounts to use a passphrase instead of simply a password.
//A passphrase consists of a series of words (lowercase letters) separated by spaces.
//To ensure security, a valid passphrase must contain no duplicate words.
//For example:
//aa bb cc dd ee is valid.
//aa bb cc dd aa is not valid - the word aa appears more than once.
//aa bb cc dd aaa is valid - aa and aaa count as different words.
//The system's full passphrase list is available as your puzzle input. How many passphrases are valid?

//Your puzzle answer was 337.
let part1 = 
    let splittedInput = input |> Seq.map (fun line -> line.Split [|' '|]) |> Seq.toList
    let rec correctInputs si acc = 
        match si with
        | []    -> acc
        | x::xs -> correctInputs xs (acc + if Seq.length x = Seq.length (Seq.distinct x) then 1 else 0)

    printfn "Part 1 result: %i" (correctInputs splittedInput 0)

part1

// -------------------------------------------------------------------------------
// ------------------------------- Part 2 ----------------------------------------
// -------------------------------------------------------------------------------
//For added security, yet another system policy has been put in place. Now, a valid passphrase must contain no two words 
//that are anagrams of each other - that is, a passphrase is invalid if any word's letters can be rearranged to form any other word in the passphrase.
//For example:
//abcde fghij is a valid passphrase.
//abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
//a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
//iiii oiii ooii oooi oooo is valid.
//oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.
//Under this new system policy, how many passphrases are valid?

//Your puzzle answer was 231.
let part2 = 
    let splittedInput = input |> Seq.map (fun line -> line.Split [|' '|] |> Seq.map (fun elem -> new string [|for c in (Seq.sort elem) -> c|])) |> Seq.toList
    let rec correctInputs si acc = 
        match si with
        | []    -> acc
        | x::xs -> correctInputs xs (acc + if Seq.length x = Seq.length (Seq.distinct x) then 1 else 0)

    printfn "Part 2 result: %i" (correctInputs splittedInput 0)

System.Console.ReadKey() |> ignore