namespace Trigrams

module Indexer =
    open System

    type Node<'a> = { First: 'a; Second: 'a; Third: 'a list }

    let index (elements: 'a list) =

        let rec index' remaining indexed =
            match remaining with
            | x1::x2::x3::xs -> index' (x2::x3::xs) (((x1, x2), x3) :: indexed)
            | _ -> indexed

        index' elements []
        |> List.groupBy (fun e -> e |> fst)
        |> List.map (fun g -> { First = g |> fst |> fst;
                                Second =  g |> fst |> snd;
                                Third = g |> snd |> List.map snd })

    let private rand' = new System.Random()
    let pickRandom list =
        rand'.Next(0, list |> List.length) 
        |> (fun i -> List.item i list)

    let rec random words firstTwo =
        seq {
            let w' = words |> List.tryFind (fun w -> w |> fst = firstTwo)
            if w'.IsSome then 
                let found = w'.Value |> snd
                yield found |> pickRandom
                for next in random words (firstTwo |> snd, found) do
                    yield next
        }