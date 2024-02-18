mod Day4 {
    use Utils.{readInputLines, resultOrQuit, Throw};

    type alias Card = {winningNumbers = Set[Int32], numbers = List[Int32]}

    pub def day4a(): Int32 \ {IO, Throw} =
        readInputLines(4) |> score

    def score(cardTexts: List[String]): Int32 =
        List.map(toCard, cardTexts)
            |> List.map(countWinningNumbers)
            |> List.map(w -> Int32.pow(2, w - 1))
            |> List.sum

    def toCard(text: String): Card =
        let split = String.splitOn({substr="|"}, text) |> List.toVector;
        {winningNumbers = Vector.get(0, split) |> toNumbers |> List.toSet, numbers = Vector.get(1, split) |> toNumbers}

    def countWinningNumbers(card: Card): Int32 =
        List.filter(n -> Set.memberOf(n, card.winningNumbers), card.numbers)
            |> List.length

    def toNumbers(item: String): List[Int32] =
        item 
            |> String.splitOn({substr=" "})
            |> List.map(Int32.parse(10))
            |> List.filter(Result.isOk)
            |> List.map(resultOrQuit)

    def testSample(): List[String] =
        List#{
            "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
            "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
            "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
            "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
            "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
        }

    @Test 
    def testScore(): Bool =
        Assert.eq(13, score(testSample()))
}