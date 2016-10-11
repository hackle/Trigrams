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
        let actual = Indexer.index []  input

        Assert.Equal<Node<int> list>(expected, actual)

    [<Fact>]
    let ``Converts list to trigrams, allowing dups for same keys`` () =
        let input = [ 1; 2; 1; 2; 3; 4 ]
        let expected = 
            [            
                { First = 2; Second = 3; Third = [ 4 ] };
                { First = 1; Second = 2; Third = [ 3; 1 ] };   
                { First = 2; Second = 1; Third = [ 2 ] };
            ]
        let actual = Indexer.index [] input

        Assert.Equal<Node<int> list>(expected, actual)
        
    [<Fact>]
    let ``Makes list of followup words`` () =
        let nodes = 
            [
                { First = 1; Second = 2; Third = [ 3 ] };
                { First = 2; Second  = 3; Third = [ 4 ] };
                { First = 3; Second  = 4; Third = [ 5 ] }
            ]

        let pickFromNodes = TState (Indexer.pickNext nodes)
        let actual = 
            Indexer.trigram {
                let! w0 = Indexer.getStateT
                Assert.Equal(2, w0)

                let! w1 = pickFromNodes
                Assert.Equal(3, w1)

                let! w2 = pickFromNodes
                Assert.Equal(4, w2)

                let! w3 = pickFromNodes
                Assert.Equal(5, w3)

                return w3
            } |> Indexer.runT (1, 2) |> snd

        Assert.Equal<int*int>((4, 5), actual)

    [<Fact>]
    let ``Skips to end on no-op`` () =
        let nodes = 
            [
                { First = 1; Second = 2; Third = [ 3 ] };
            ]

        let pickFromNodes = TState (Indexer.pickNext nodes)
        let actual = 
            Indexer.trigram {
                let! w1 = pickFromNodes
                Assert.Equal(3, w1)

                let! w2 = pickFromNodes
                // these should never run
                Assert.Equal(4, w2)

                let! w3 = pickFromNodes
                Assert.Equal(5, w3)

                return w3
            } |> Indexer.runT (1, 2) |> snd

        Assert.Equal<int*int>((2, 3), actual)