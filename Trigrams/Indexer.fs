﻿namespace Trigrams

module Indexer =
    open System

    type Node<'a> = { First: 'a; Second: 'a; Third: 'a list }

    let index skips (elements: 'a list) =
        let shouldSkip c =
            List.contains c skips

        let rec index' remaining indexed =
            match remaining with
            | x1::x2::x3::xs ->
                let carry = ((x1, x2), x3) :: indexed
                index' (x2::x3::xs) carry
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

    let takeMax n list =
        list
        |> List.take (list |> List.length |> min n)

    let pickMostUsed list =
        let pick' ls = 
            ls 
            |> takeMax 3
            |> pickRandom
        
        list
        |> List.groupBy id
        |> List.sortByDescending (fun e -> e |> snd |> List.length)
        |> pick'
        |> fst
        
    type TrigramState<'a> = TState of (('a * 'a) -> 'a option * ('a * 'a))
    let runT p (TState s) = s p
    let getStateT = 
        let getS' s = Some (s |> snd), s
        TState getS'

    let pickNext (nodes: Node<'a> list) (key: 'a * 'a) =
        let n' = nodes |> List.tryFind (fun w -> (w.First, w.Second) = key)
        match n' with
        | None -> None, key
        | Some x -> 
            let found = x.Third |> pickRandom
            Some found, (x.Second, found)

    type TrigramStateBuilder() = 
        member this.Bind((TState x), (f: ('a -> TrigramState<'a>))) =
            let runState p =
                let (ro, state') = x p
                match ro with
                | None -> None, state'
                | Some r1 -> 
                    f r1 |> runT state'
            TState runState

        member this.Return(x) = 
            let returnT p = Some x, p
            TState returnT

    let trigram = new TrigramStateBuilder()