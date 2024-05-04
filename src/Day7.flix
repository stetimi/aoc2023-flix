mod Day7 {
    use Utils.{assertEq, listSum, readInputLines, Throw, parseInt64OrThrow, negateComparison}

    type alias Hand = Vector[Int64]
    enum HandType with Eq, Order, ToString {
        case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
    }

    pub def part1(): Int64 \ {IO, Throw} =
        readInputLines("Day7")
            |> List.map(parseLine)
            |> rank(noJokers, scoreNoJokers)

    pub def part2(): Int64 \ {IO, Throw} =
        readInputLines("Day7")
            |> List.map(parseLine)
            |> rank(jackIsJoker, jackScoresLow)        

    def noJokers(_: Int64): Bool = false
    def jackIsJoker(c: Int64): Bool = c == 11i64
    def scoreNoJokers(c: Int64): Int64 = c
    def jackScoresLow(c: Int64): Int64 = 
        if (c == 11i64) {
            0i64
        } else {
            c
        }

    def rank(isJoker: Int64 -> Bool, jokerScore: Int64 -> Int64, hands: List[(Hand, Int64)]): Int64 =
        hands
            |> List.map(match (handType, hand) -> ( { handType = identifyHandType(handType, isJoker), score = score(handType, jokerScore) }, hand) )
            |> List.sortBy(match ({handType, score}, _) -> (handType, score))
            |> List.map(snd)
            |> List.zipWithIndex
            |> List.map(match (i, s) -> Int32.toInt64(i + 1) * s)
            |> listSum(0i64)

    pub def score(hand: Hand, mapScore: Int64 -> Int64): Int64 =
        hand 
            |> Vector.map(mapScore)
            |> Vector.foldLeft((acc, s) -> acc * 16i64 + s, 0i64)
        
    def compareByCardRank(hand1: Hand, hand2: Hand): Comparison =
        Vector.zip(hand1, hand2)
            |> Vector.findLeft(match (h1, h2) -> h1 != h2)
            |> Option.map(match (h1, h2) -> Int64.compare(h1, h2) |> Comparison.fromInt32)
            |> Option.getWithDefault(Comparison.EqualTo)

    def identifyHandType(h: Hand, isJoker: Int64 -> Bool): HandType =
        let grouped = h 
            |> Vector.filter(c -> not isJoker(c))
            |> Vector.groupBy((x, y) -> x == y) 
            |> Vector.map(Vector.length) 
            |> Vector.sort 
            |> Vector.reverse;
        def count(i) = Vector.get(i, grouped);
        let jokersCount = Vector.count(isJoker, h);
        let largestGroupSize = if (Vector.isEmpty(grouped)) {
            0
        } else {
            Vector.get(0, grouped)
        };
        if (largestGroupSize + jokersCount == 5) {
            HandType.FiveOfAKind
        } else if (largestGroupSize + jokersCount == 4) {
            HandType.FourOfAKind
        } else if (largestGroupSize + jokersCount == 3) {
            if (count(1) == 2) {
                HandType.FullHouse
            } else {
                HandType.ThreeOfAKind
            }
        } else if (largestGroupSize == 2 and count(1) == 2) {
            HandType.TwoPair
        } else if (largestGroupSize == 2 or jokersCount != 0) {
            HandType.OnePair
        } else {
            HandType.HighCard
        }

    def parseLine(line: String): (Hand, Int64) \ Throw =
        let split = line
            |> String.splitOn({substr=" "})
            |> List.toVector;
        (Vector.get(0, split) |> toHand, Vector.get(1, split) |> parseInt64OrThrow)

    pub def toHand(text: String): Hand \ Throw  =
        text
            |> String.toVector
            |> Vector.map(ch -> match ch {
                case 'A' => 14i64
                case 'K' => 13i64
                case 'Q' => 12i64
                case 'J' => 11i64
                case 'T' => 10i64
                case _ => Char.toString(ch) |> parseInt64OrThrow
            })

    @Test 
    def testParseLine(): Bool =
        assertEq((Vector#{3i64, 2i64, 10i64, 3i64, 13i64}, 765i64), () -> parseLine("32T3K 765"))

    @Test
    def identifiesFiveOfAKind(): Bool =
        assertEq(HandType.FiveOfAKind, () -> identifyHandType(toHand("QQQQQ"), noJokers))

    @Test
    pub def identifiesFourOfAKind(): Bool =
        assertEq(HandType.FourOfAKind, () -> identifyHandType(toHand("QQAQQ"), noJokers))

    @Test
    def identifiesFullHouse(): Bool =
        assertEq(HandType.FullHouse, () -> identifyHandType(toHand("QQAAQ"), noJokers))

    @Test
    def identifiesThreeOfAKind(): Bool =
        assertEq(HandType.ThreeOfAKind, () -> identifyHandType(toHand("QQA2Q"), noJokers))   

    @Test
    def identifiesTwoPair(): Bool =
        assertEq(HandType.TwoPair, () -> identifyHandType(toHand("QQA22"), noJokers))  

    @Test
    def identifiesOnePair(): Bool =
        assertEq(HandType.OnePair, () -> identifyHandType(toHand("QQA72"), noJokers))  

    @Test
    def identifiesHighCard(): Bool =
        assertEq(HandType.HighCard, () -> identifyHandType(toHand("KQA72"), noJokers))  

    @Test
    pub def identifiesFiveOfAKindWithAllJokers(): Bool =
        assertEq(HandType.FiveOfAKind, () -> identifyHandType(toHand("JJJJJ"), jackIsJoker))  

    @Test
    pub def identifiesFiveOfAKindWithJokers(): Bool =
        assertEq(HandType.FiveOfAKind, () -> identifyHandType(toHand("JJ88J"), jackIsJoker))  

    @Test
    pub def identifiesFourOfAKindWithJokers(): Bool =
        assertEq(HandType.FourOfAKind, () -> identifyHandType(toHand("JJ87J"), jackIsJoker)) 

    @Test
    pub def identifiesThreeOfAKindWithOneJoker(): Bool =
        assertEq(HandType.ThreeOfAKind, () -> identifyHandType(toHand("J8442"), jackIsJoker))  

    @Test
    pub def identifiesThreeOfAKindWithTwoJokers(): Bool =
        assertEq(HandType.ThreeOfAKind, () -> identifyHandType(toHand("JJ472"), jackIsJoker))  

     @Test
    pub def identifiesTwoPairWithJokersAllowed(): Bool =
        assertEq(HandType.TwoPair, () -> identifyHandType(toHand("4466A"), jackIsJoker)) 

    @Test
    pub def identifiesOnePairWithOneJoker(): Bool =
        assertEq(HandType.OnePair, () -> identifyHandType(toHand("J4567"), jackIsJoker))                     
    
    @Test
    pub def identifiesOnePairWithJokersAlloed(): Bool =
        assertEq(HandType.OnePair, () -> identifyHandType(toHand("44567"), jackIsJoker))  
    
    @Test
    pub def identifiesHighCardWithJokersAllowed(): Bool =
        assertEq(HandType.HighCard, () -> identifyHandType(toHand("4T68A"), jackIsJoker))                     

    @Test 
    def testCompareByCardRankWhenEqual(): Bool =
        assertCompareByCardRank(Comparison.EqualTo, "A98T7", "A98T7")

    @Test 
    def testCompareByCardRankWhenAllSameExceptForLast(): Bool =
       assertCompareByCardRank(Comparison.LessThan, "A98T6", "A98T7")

    @Test 
    def testCompareByCardRankWhenAllSameExceptForFirst(): Bool =
       assertCompareByCardRank(Comparison.GreaterThan, "A98T6", "K98T6")  

    @Test 
    def testCompareByCardRank1(): Bool =
        assertCompareByCardRank(Comparison.LessThan, "KTJJT", "KK677") 

    def assertCompareByCardRank(expected: Comparison, h1: String, h2: String): Bool =
        assertEq(expected, () -> compareByCardRank(toHand(h1), toHand(h2))) 
        and assertEq(negateComparison(expected), () -> compareByCardRank(toHand(h2), toHand(h1)))

    @Test 
    pub def testPart1(): Bool =
        assertEq(6440i64, 
            () -> readInputLines("Day7Test") 
                |> List.map(parseLine)
                |> rank(noJokers, scoreNoJokers))

     @Test 
    pub def testPart2(): Bool =
        assertEq(5905i64, 
            () -> readInputLines("Day7Test") 
                |> List.map(parseLine)
                |> rank(jackIsJoker, jackScoresLow))       
}