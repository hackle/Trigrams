namespace Trigrams

module IndexerTests =
    open Xunit

    [<Fact>]
    let ``Converts list to trigrams`` () =
        let input = [ 1 .. 4 ]
        let expected = 
            [
                (2, 3, 4);
                (1, 2, 3)
            ]
        let actual = Indexer.index input

        Assert.Equal<(int * int * int) list>(expected, actual)