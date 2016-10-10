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

    let rec random (key: 'a * 'a) (nodes: Node<'a> list) =
        seq {
            let n' = nodes |> List.tryFind (fun w -> (w.First, w.Second) = key)
            if n'.IsSome then 
                let found = n'.Value.Third |> pickRandom
                yield found
                for next in random (key |> snd, found) nodes do
                    yield next
        }