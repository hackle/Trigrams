// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Trigrams

[<EntryPoint>]
let main argv = 
    printfn "File name: "
    let fileName = @"E:\work\F#\great expectations.txt" //System.Console.ReadLine()
    printfn "First 2 words: "
    let first2Words = System.Console.ReadLine().Split([| ' ' |], 2)
    printfn "Count of words:"
    let length = System.Console.ReadLine() |> int
    
    let wordRegex = new System.Text.RegularExpressions.Regex(@"[a-z']+|[\.\,;\!]", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    let skips = [ "."; ","; ";"; "!" ]

    let firstWord =
        fileName
        |> System.IO.File.ReadAllText
        |> wordRegex.Matches
        |> (fun ms -> [ for m in ms do yield m ])
        |> List.ofSeq
        |> List.map (fun w -> w.Value)
        |> Indexer.index skips
        |> (fun nodes -> Indexer.pickNext nodes (first2Words.[0], first2Words.[1]))

    printfn "Here we go (press space to get more words)"
    printf "%s %s" first2Words.[0] first2Words.[1]
    
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
