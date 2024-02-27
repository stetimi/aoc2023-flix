mod Day5 {
    use Utils.{getOrQuit, listSplit, readInputLines, resultOrQuit, uncheckedReadInputLines, unsafeHead, unsafeParseInt64, Throw};

    type alias ResourceMapping = {dest = Int64, src = Int64, len = Int64}
    type alias ResourceMappings = List[ResourceMapping]

    def matchingResouceMapping(n: Int64, ms: ResourceMappings): Int64 =
        List.findLeft(rm -> rm.src <= n and n < rm.src + rm.len, ms) 
            |> Option.map(rm -> n + (rm.dest - rm.src))
            |> Option.getWithDefault(n)

    def chainResourceMappings(ms: List[ResourceMappings], n: Int64): Int64 =
        def go(ms2: List[ResourceMappings], n2) = {
            if (List.isEmpty(ms2)) {
                n2
            } else {
                unsafeHead(ms2)
                    |> matchingResouceMapping(n2)
                    |> go(List.drop(1, ms2))
            }
        };
        go(ms, n)
        
    pub def part1(): Int64 \ {IO, Throw} =
        readInputLines(5)
            |> lowestLocationNumber

    def lowestLocationNumber(lines: List[String]): Int64 =
        let seeds = unsafeHead(lines)
            |> String.splitOn({substr=" "})
            |> List.drop(1)
            |> List.map(unsafeParseInt64);
        let maps = List.drop(2, lines)
            |> listSplit(s -> String.trim(s) |> String.isEmpty)
            |> List.map(parseResourceMappings);
        List.map(chainResourceMappings(maps), seeds)
            |> List.minimum
            |> getOrQuit

    pub def parseResourceMappings(lines: List[String]): ResourceMappings = 
        List.drop(1, lines)
            |> List.map(parseResourceMapping)

    def parseResourceMapping(line: String): ResourceMapping = 
        let split = line
            |> String.splitOn({substr=" "})
            |> List.toVector;
        def get(i) = Vector.get(i, split) |> Int64.parse(10) |> resultOrQuit("Cannot parse '${line}, index ${i}'");
        {dest = get(0), src = get(1), len = get(2)}
        

    @Test 
    def testPart1(): Bool =
        let lines = uncheckedReadInputLines("Day5Test");
        Assert.eq(35i64, lowestLocationNumber(lines))

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