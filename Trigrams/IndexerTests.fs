namespace Trigrams

module IndexerTests =
    open Xunit
    open Indexer

    [<Fact>]
    let ``Converts list to trigrams`` () =
        let input = [ 1 .. 4 ]
        let expected = 
            [
                { First = 2; Second = 3; Third = [ 4 ] };
                { First = 1; Second = 2; Third = [ 3 ] }
            ]
        let actual = Indexer.index input

        Assert.Equal<Node<int> list>(expected, actual)

    [<Fact>]
    let ``Converts list to trigrams, allowing dups for same keys`` () =
        let input = [ 1; 2; 1; 2; 3 ]
        let expected = 
            [
                { First = 1; Second = 2; Third = [ 3; 1 ] };   
                { First = 2; Second = 1; Third = [ 2 ] }
            ]
        let actual = Indexer.index input

        Assert.Equal<Node<int> list>(expected, actual)
        
    [<Fact>]
    let ``Makes list of followup words`` () =
        let nodes = 
            [
                { First = 1; Second = 2; Third = [ 3 ] };
                { First = 2; Second  = 3; Third = [ 4 ] };
                { First = 3; Second  = 4; Third = [ 5 ] }
            ]

        let actual = Indexer.make (1, 2) nodes |> List.ofSeq
        let expected = [ 3; 4; 5 ]
        Assert.Equal<int seq>(expected, actual)

    [<Fact>]
    let ``Chooses the most frequent word from Third`` () =
        let nodes = 
            [
                { First = 1; Second = 2; Third = [ 3; 2; 1; 3 ] };
                { First = 2; Second  = 3; Third = [ 4; 3; 4; 4; 3 ] };
                { First = 3; Second  = 4; Third = [ 5; 8; 7; 5; 8; 7; 5 ] }
            ]

        let actual = Indexer.make (1, 2) nodes |> List.ofSeq
        let expected = [ 3; 4; 5 ]
        Assert.Equal<int seq>(expected, actual)