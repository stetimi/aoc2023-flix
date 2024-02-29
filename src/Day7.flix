mod Day7 {
    use Utils.{debugF, listSum, uncheckedReadInputLines, unsafeParseInt32, unsafeParseInt64, toStringDebug, negateComparison, debugF}

    type alias Hand = Vector[Int64]
    enum HandType with Eq, Order, ToString {
        case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
    }

    pub def part1(): Int64 =
        uncheckedReadInputLines("Day7")
            |> List.map(parseLine)
            |> rank

    def rank(hands: List[(Hand, Int64)]): Int64 =
        hands
            |> List.map(hr -> ( { handType = identifyHandType(fst(hr)), score = score(fst(hr)) }, snd(hr)) )
            |> List.sortBy(hsh -> (fst(hsh).handType, fst(hsh).score))
            |> List.map(snd)
            |> List.zipWithIndex
            |> List.map(x -> (fst(x) + 1, snd(x)))
            |> List.map(xy -> Int32.toInt64(fst(xy)) * snd(xy))
            |> listSum(0i64)

    pub def score(hand: Hand): Int64 =
        def powTimes(i: Int32) = Int64.pow(16i64, Int32.toInt64(4 - i)) * Vector.get(i, hand);
        powTimes(0) + powTimes(1) + powTimes(2) + powTimes(3) + powTimes(4)
        
    def compareByCardRank(h1: Hand, h2: Hand): Comparison =
        Vector.zip(h1, h2)
            |> Vector.findLeft(x -> fst(x) != snd(x))
            |> Option.map(x -> Int64.compare(fst(x), snd(x)) |> Comparison.fromInt32)
            |> Option.getWithDefault(Comparison.EqualTo)

    def identifyHandType(h: Hand): HandType =
        let grouped = Vector.groupBy((x, y) -> x == y, h) |> Vector.map(Vector.length) |> Vector.sort;
        def count(i) = Vector.get(i, grouped);
        let distinctCount = Vector.length(grouped);
        match distinctCount {
            case 1 => HandType.FiveOfAKind
            case 2 => if (count(1) == 4) {
                HandType.FourOfAKind
            } else {
                HandType.FullHouse
            }
            case 3 => if (count(2) == 3) {
                HandType.ThreeOfAKind
            } else {
                HandType.TwoPair
            }
            case _ => if (distinctCount == 4) {
                HandType.OnePair
            } else {
                 HandType.HighCard
            }
        }

    def parseLine(line: String): (Hand, Int64) =
        let split = line
            |> String.splitOn({substr=" "})
            |> List.toVector;
        (toHand(Vector.get(0, split)), unsafeParseInt64(Vector.get(1, split)))

    pub def toHand(text: String): Hand =
        def toCard(ch) = 
            match ch {
                case 'A' => 14
                case 'K' => 13
                case 'Q' => 12
                case 'J' => 11
                case 'T' => 10
                case _ => Char.toString(ch) |> unsafeParseInt32
            } |> Int32.toInt64;
        text
            |> String.toVector
            |> Vector.map(toCard)

    @Test 
    def testParseLine(): Bool =
        Assert.eq((Vector#{3i64, 2i64, 10i64, 3i64, 13i64}, 765i64), parseLine("32T3K 765"))

    @Test
    def identifiesFiveOfAKind(): Bool =
        Assert.eq(HandType.FiveOfAKind, identifyHandType(toHand("QQQQQ")))

    @Test
    def identifiesFourOfAKind(): Bool =
        Assert.eq(HandType.FourOfAKind, identifyHandType(toHand("QQAQQ")))

    @Test
    def identifiesFullHouse(): Bool =
        Assert.eq(HandType.FullHouse, identifyHandType(toHand("QQAAQ")))

    @Test
    def identifiesThreeOfAKind(): Bool =
        Assert.eq(HandType.ThreeOfAKind, identifyHandType(toHand("QQA2Q")))   

    @Test
    def identifiesTwoPair(): Bool =
        Assert.eq(HandType.TwoPair, identifyHandType(toHand("QQA22")))  

    @Test
    def identifiesOnePair(): Bool =
        Assert.eq(HandType.OnePair, identifyHandType(toHand("QQA72")))  

    @Test
    def identifiesHighCard(): Bool =
        Assert.eq(HandType.HighCard, identifyHandType(toHand("KQA72")))  

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
            |> rank;
        Assert.eq(6440i64, ans)
}