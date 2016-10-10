namespace Trigrams

module Indexer =
    open System

    let index (elements: 'a list) =

        let rec index' remaining indexed =
            match remaining with
            | x1::x2::x3::xs -> index' (x2::x3::xs) ((x1, x2, x3) :: indexed)
            | _ -> indexed

        index' elements []