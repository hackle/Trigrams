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