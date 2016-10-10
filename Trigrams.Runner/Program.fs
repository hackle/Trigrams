// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Trigrams

[<EntryPoint>]
let main argv = 
    printfn "File name: "
    let fileName = @"E:\work\F#\great expectations.txt" //System.Console.ReadLine()
    printfn "First 2 words: "
    let first2Words = System.Console.ReadLine().Split([| ' ' |], 2)

    let wordRegex = new System.Text.RegularExpressions.Regex(@"[a-z]+|[\.\,;\!]", System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    
    let words =
        fileName
        |> System.IO.File.ReadAllText
        |> wordRegex.Matches
        |> (fun ms -> [ for m in ms do yield m ])
        |> List.ofSeq
        |> List.map (fun w -> w.Value)
        |> Indexer.index
        |> Indexer.random (first2Words.[0], first2Words.[1])

    printfn "Here we go (press space to get more words)"
    printf "%s %s" first2Words.[0] first2Words.[1]

    let rec output ws = 
        if ' ' = System.Console.ReadKey(true).KeyChar then
            if ws |> Seq.isEmpty
            then printf "(END)"
            else
                printf " %s" (ws |> Seq.head)
                output (ws |> Seq.tail)
        else printf "(END)"

    output words

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
