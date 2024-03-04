mod Day7 {
    use Utils.{debugF, listSum, uncheckedReadInputLines, unsafeParseInt32, unsafeParseInt64, toStringDebug, negateComparison, debugF}

    type alias Hand = Vector[Int64]
    enum HandType with Eq, Order, ToString {
        case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
    }

    pub def part1(): Int64 =
        uncheckedReadInputLines("Day7")
            |> List.map(parseLine)
            |> rank(noJokers, scoreNoJokers)

    pub def part2(): Int64 =
        uncheckedReadInputLines("Day7")
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

    def parseLine(line: String): (Hand, Int64) =
        let split = line
            |> String.splitOn({substr=" "})
            |> List.toVector;
        (Vector.get(0, split) |> toHand, Vector.get(1, split) |> unsafeParseInt64)

    pub def toHand(text: String): Hand =
        text
            |> String.toVector
            |> Vector.map(ch -> match ch {
                case 'A' => 14i64
                case 'K' => 13i64
                case 'Q' => 12i64
                case 'J' => 11i64
                case 'T' => 10i64
                case _ => Char.toString(ch) |> unsafeParseInt64
            })

    @Test 
    def testParseLine(): Bool =
        Assert.eq((Vector#{3i64, 2i64, 10i64, 3i64, 13i64}, 765i64), parseLine("32T3K 765"))

    @Test
    def identifiesFiveOfAKind(): Bool =
        Assert.eq(HandType.FiveOfAKind, identifyHandType(toHand("QQQQQ"), noJokers))

    @Test
    pub def identifiesFourOfAKind(): Bool =
        Assert.eq(HandType.FourOfAKind, identifyHandType(toHand("QQAQQ"), noJokers))

    @Test
    def identifiesFullHouse(): Bool =
        Assert.eq(HandType.FullHouse, identifyHandType(toHand("QQAAQ"), noJokers))

    @Test
    def identifiesThreeOfAKind(): Bool =
        Assert.eq(HandType.ThreeOfAKind, identifyHandType(toHand("QQA2Q"), noJokers))   

    @Test
    def identifiesTwoPair(): Bool =
        Assert.eq(HandType.TwoPair, identifyHandType(toHand("QQA22"), noJokers))  

    @Test
    def identifiesOnePair(): Bool =
        Assert.eq(HandType.OnePair, identifyHandType(toHand("QQA72"), noJokers))  

    @Test
    def identifiesHighCard(): Bool =
        Assert.eq(HandType.HighCard, identifyHandType(toHand("KQA72"), noJokers))  

    @Test
    pub def identifiesFiveOfAKindWithAllJokers(): Bool =
        Assert.eq(HandType.FiveOfAKind, identifyHandType(toHand("JJJJJ"), jackIsJoker))  

    @Test
    pub def identifiesFiveOfAKindWithJokers(): Bool =
        Assert.eq(HandType.FiveOfAKind, identifyHandType(toHand("JJ88J"), jackIsJoker))  

    @Test
    pub def identifiesFourOfAKindWithJokers(): Bool =
        Assert.eq(HandType.FourOfAKind, identifyHandType(toHand("JJ87J"), jackIsJoker)) 

    @Test
    pub def identifiesThreeOfAKindWithOneJoker(): Bool =
        Assert.eq(HandType.ThreeOfAKind, identifyHandType(toHand("J8442"), jackIsJoker))  

    @Test
    pub def identifiesThreeOfAKindWithTwoJokers(): Bool =
        Assert.eq(HandType.ThreeOfAKind, identifyHandType(toHand("JJ472"), jackIsJoker))  

     @Test
    pub def identifiesTwoPairWithJokersAllowed(): Bool =
        Assert.eq(HandType.TwoPair, identifyHandType(toHand("4466A"), jackIsJoker)) 

    @Test
    pub def identifiesOnePairWithOneJoker(): Bool =
        Assert.eq(HandType.OnePair, identifyHandType(toHand("J4567"), jackIsJoker))                     
    
    @Test
    pub def identifiesOnePairWithJokersAlloed(): Bool =
        Assert.eq(HandType.OnePair, identifyHandType(toHand("44567"), jackIsJoker))  
    
    @Test
    pub def identifiesHighCardWithJokersAllowed(): Bool =
        Assert.eq(HandType.HighCard, identifyHandType(toHand("4T68A"), jackIsJoker))                     

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
        Assert.eq(expected, compareByCardRank(toHand(h1), toHand(h2))) 
        and Assert.eq(negateComparison(expected), compareByCardRank(toHand(h2), toHand(h1)))

    @Test 
    pub def testPart1(): Bool =
        let ans = uncheckedReadInputLines("Day7Test") 
            |> List.map(parseLine)
            |> rank(noJokers, scoreNoJokers);
        Assert.eq(6440i64, ans)

     @Test 
    pub def testPart2(): Bool =
        let ans = uncheckedReadInputLines("Day7Test") 
            |> List.map(parseLine)
            |> rank(jackIsJoker, jackScoresLow);
        Assert.eq(5905i64, ans)       
}