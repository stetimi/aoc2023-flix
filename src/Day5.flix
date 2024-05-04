mod Day5 {
    use Utils.{assertEq, getOrThrow, headOrThrow, listSplit, readInputLines, resultOrThrow, parseInt64OrThrow, Throw};

    type alias ResourceMapping = {dest = Int64, src = Int64, len = Int64}
    type alias ResourceMappings = List[ResourceMapping]

    def matchingResouceMapping(n: Int64, ms: ResourceMappings): Int64 =
        List.findLeft(rm -> rm.src <= n and n < rm.src + rm.len, ms) 
            |> Option.map(rm -> n + (rm.dest - rm.src))
            |> Option.getWithDefault(n)

    def chainResourceMappings(ms: List[ResourceMappings], n: Int64): Int64 =
        def go(ms2, n2) = 
            match ms2 {
                case Nil => n2
                case h :: rest => matchingResouceMapping(n2, h) |> go(rest)
            };
        go(ms, n)
        
    pub def part1(): Int64 \ {IO, Throw} =
        readInputLines("Day5")
            |> lowestLocationNumber

    def lowestLocationNumber(lines: List[String]): Int64 \ Throw =
        let seeds = headOrThrow(lines)
            |> String.splitOn({substr=" "})
            |> List.drop(1)
            |> List.map(parseInt64OrThrow);
        let maps = List.drop(2, lines)
            |> listSplit(s -> String.trim(s) |> String.isEmpty)
            |> List.map(parseResourceMappings);
        List.map(chainResourceMappings(maps), seeds)
            |> List.minimum
            |> getOrThrow

    pub def parseResourceMappings(lines: List[String]): ResourceMappings \ Throw = 
        List.drop(1, lines)
            |> List.map(parseResourceMapping)

    def parseResourceMapping(line: String): ResourceMapping \ Throw = 
        let split = line
            |> String.splitOn({substr=" "})
            |> List.toVector;
        def get(i) = Vector.get(i, split) |> Int64.parse(10) |> resultOrThrow;
        {dest = get(0), src = get(1), len = get(2)}
        

    @Test 
    def testPart1(): Bool =
        assertEq(35i64, () -> readInputLines("Day5Test") |> lowestLocationNumber)

    def sampleResourceMappings(): ResourceMappings =
        List#{
            {dest = 20i64, src = 10i64, len = 2i64},
            {dest = 30i64, src = 20i64, len = 5i64}
        }

    @Test 
    def matchingResouceMappingWhenMatch(): Bool =
        Assert.eq(34i64, matchingResouceMapping(24i64, sampleResourceMappings()))

    @Test 
    def matchingResouceMappingWhenNoMatch(): Bool =
        Assert.eq(25i64, matchingResouceMapping(25i64, sampleResourceMappings()))
}