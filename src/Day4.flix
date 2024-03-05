mod Day4 {
    use Utils.{assertEq, doIf, readInputLines, resultOrThrow, Throw};

    enum Card({winningNumbers = Set[Int32], numbers = List[Int32]})

    pub def day4a(): Int32 \ {IO, Throw} =
        readInputLines("Day4") |> score1

    pub def day4b(): Int32 \ {IO, Throw} =
        readInputLines("Day4") |> score2

    def score1(cardTexts: List[String]): Int32 \ Throw =
        cardTexts
            |> List.map(toCard >> countWinningNumbers)
            |> List.map(w -> Int32.pow(2, w - 1))
            |> List.sum

    def score2(cardTexts: List[String]): Int32 \ Throw =
        let cards = cardTexts |> List.toVector |> Vector.map(toCard);
        region r {
            let counts = Array.repeat(r, Vector.length(cards), 1);
            def incFrom!(start: Int32, end: Int32, amount: Int32): Unit =
                doIf(start <= end, () -> 
                    let value = Array.get(start, counts) + amount;
                    Array.put(value, start, counts);
                    incFrom!(start + 1, end, amount)
                );
            def play(index: Int32): Unit = 
                doIf(index != Vector.length(cards), () -> 
                    let top = Vector.get(index, cards);
                    let winCount = countWinningNumbers(top);
                    let amount = Array.get(index, counts);
                    incFrom!(index + 1, index + winCount, amount);
                    play(index + 1)
                );
            play(0);
            Array.sum(counts)
        }

    def toCard(text: String): Card \ Throw =
        let split = String.splitOn({substr="|"}, text) |> List.map(toNumbers) |> List.toVector;
        def get(i) = Vector.get(i, split);
        Card.Card({winningNumbers = get(0) |> List.toSet, numbers = get(1)})

    def countWinningNumbers(card: Card): Int32 =
        let Card.Card(c) = card;
        List.count(n -> Set.memberOf(n, c.winningNumbers), c.numbers)

    def toNumbers(item: String): List[Int32] \ Throw  =
        item 
            |> String.splitOn({substr=" "})
            |> List.map(Int32.parse(10))
            |> List.filter(Result.isOk)
            |> List.map(resultOrThrow)

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
        assertEq(13, () -> score1(testSample()))

    @Test 
    def testPlay(): Bool =
        assertEq(30, () -> score2(testSample()))
}
