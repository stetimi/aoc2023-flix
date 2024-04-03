mod Day12 {
    use Utils.{assertEq, readInputLines, parseInt32OrThrow, stringForEach, listSum, vectorSum, getOrThrow, Throw}

    type alias ParsedLine = {springs = String, criteria = Vector[Int32]}

    pub def part1(): Int64 \ {IO, Throw} = 
        run(readInputLines("Day12") |> List.map(parseLine), countConditionRecords)

    pub def part2(): Int64 \ {IO, Throw} = 
        run(readInputLines("Day12") |> List.map(parseLine) |> List.map(expandParsedLine), countConditionRecords)

    pub def run(lines: List[ParsedLine], part: ParsedLine -> Int64 \ e): Int64 \ e =
        lines |> List.map(part) |> listSum(0i64)

    def parseLine(line: String): ParsedLine \ Throw =
        let split = String.splitOn(substr=" ", line) |> List.toVector;
        let springs = Vector.get(0, split);
        let criteriaString = Vector.get(1, split);
        let criteria = String.splitOn(substr=",", criteriaString)
            |> List.map(parseInt32OrThrow)
            |> List.toVector;
        {springs=springs, criteria=criteria}
        
    def countConditionRecords(line: ParsedLine): Int64 = region rc {
        let memo = MutMap.empty(rc);
        def loop(springs, criteria) = {
            if (Vector.isEmpty(criteria)) {
                if (not String.contains(substr="#", springs)) 1i64 else 0i64
            } else {
                def memoize(r) = {
                    MutMap.put!((springs, criteria), r, memo);
                    r
                };
                match MutMap.get((springs, criteria), memo) {
                    case Some(x) => x
                    case None => {
                        let minRemaining = vectorSum(0i32, criteria) + Vector.length(criteria) - 1;
                        let springsLen = String.length(springs);
                        if (springsLen < minRemaining) {
                            0i64
                        } else if (String.charAt(0, springs) == '.') {
                            loop(String.drop(1, springs), criteria)
                        } else {
                            let currentCriterion = Vector.get(0, criteria);
                            let allSpringsValid = String.slice(start=0, end=currentCriterion, springs) |> String.forAll(ch -> ch != '.');
                            def lastCharValid() = springsLen == currentCriterion or String.charAt(currentCriterion, springs) != '#';
                            let validCount = if (allSpringsValid and lastCharValid()) {
                                let maxIndex = Int32.min(springsLen, currentCriterion + 1);
                                loop(String.drop(maxIndex, springs), Vector.drop(1, criteria))
                            } else {
                                0i64
                            };
                            let others = if (String.charAt(0, springs) != '#') {
                                loop(String.drop(1, springs), criteria)
                            } else {
                                0i64
                            };
                            (validCount + others) |> memoize
                        }
                    }
                }
            }
        };
        loop(line.springs, line.criteria)
    }

    def expandParsedLine(line: ParsedLine): ParsedLine = 
        def expandSprings(s) = "${s}?${s}?${s}?${s}?${s}";
        def expandCriteria(s) = s ++ s ++ s ++ s ++ s;
        {springs=expandSprings(line.springs), criteria=expandCriteria(line.criteria)}


    @Test 
    pub def testPart1(): Bool \ IO =
        assertEq(21i64, () -> run(readInputLines("Day12Test") |> List.map(parseLine), countConditionRecords))
    
    @Test 
    pub def testPart2(): Bool \ IO =
        assertEq(525152i64, () -> run(readInputLines("Day12Test") |> List.map(parseLine) |> List.map(expandParsedLine), countConditionRecords))

    @Test
    def countConditionRecords1(): Bool =
        let line = {springs=".??..??...?##.", criteria=Vector#{1,1,3}};
        Assert.eq(4i64, countConditionRecords(line))

    @Test
    def countConditionRecords2(): Bool =
        let line = {springs="?###????????", 
                    criteria=Vector#{3,2,1}};
        Assert.eq(10i64, countConditionRecords(line))

    @Test
    def countConditionRecords3(): Bool =
        let line = {springs="????.######..#####.?????.######..#####.?????.######..#####.?????.######..#####.?????.######..#####.", 
                    criteria=Vector#{1,6,5,1,6,5,1,6,5,1,6,5,1,6,5}};
        Assert.eq(2500i64, countConditionRecords(line))
}