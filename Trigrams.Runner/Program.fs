// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Trigrams

[<EntryPoint>]
let main argv = 
    printfn "File name: "
    let fileName = System.Console.ReadLine()
    printfn "First 2 words: "
    let first2Words = System.Console.ReadLine().Split([| ' ' |], 2)
    
    let wordRegex = new System.Text.RegularExpressions.Regex(@"[a-z']+|[\.\,;\!]", System.Text.RegularExpressions.RegexOptions.IgnoreCase)

    let nodes =
        fileName
        |> System.IO.File.ReadAllText
        |> wordRegex.Matches
        |> (fun ms -> [ for m in ms do yield m.Value ])
        |> List.ofSeq
        |> Indexer.index

    printfn "Here we go (press space to get more words)"
    printf "%s %s" first2Words.[0] first2Words.[1]
    
    let pickFromNodes = Indexer.TState (Indexer.pickNext nodes)
    Indexer.trigram {
        while ' ' = System.Console.ReadKey().KeyChar do
            let! x = pickFromNodes
            x |> printf "%s"
    } |> Indexer.runT (first2Words.[0], first2Words.[1]) |> ignore
    
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
