mod Day6 {
    use Utils.{loop, prod, readInputLines, Throw}

    type alias Race = {time = Int32, distance = Int32}

    pub def part1(): Int32 \ {Throw, IO} =
        readInputLines(6)
            |> parseInputs
            |> winCountsAllRaces

    def winCountsAllRaces(races: List[Race]): Int32 =
        let counts = List.map(winCounts, races);
        prod(counts)

    def winCounts(race: Race): Int32 =
        region rc {
            let count = ref 0 @ rc;
            loop(0, race.distance, buttonHeld -> 
                let d = distance(race.time, buttonHeld);
                if (d > race.distance) {
                    count := (deref count) + 1
                } else {
                    ()
                };
                true
            );
            deref count
        }

    def distance(raceTime: Int32, buttonHeld: Int32): Int32 =
        let speed = buttonHeld;
        (raceTime - buttonHeld) * speed

    def parseInputs(lines: List[String]): List[Race] =
        let linesVec = List.toVector(lines);
        def parseNumbers(line: String): List[Int32] =
            String.splitOn({substr = " "}, line)
            |> List.map(Int32.parse(10))
            |> List.filterMap(Result.toOption);
        let times = parseNumbers(Vector.get(0, linesVec));
        let distances = parseNumbers(Vector.get(1, linesVec));
        List.zip(times, distances)
            |> List.map(td -> {time = fst(td), distance = snd(td)})

    def parseInputsForTest(): List[Race] =
        let lines = List#{
            "Time:      7  15   30",
            "Distance:  9  40  200"
        };
        parseInputs(lines)

    @Test
    def parseInputsTimesAreCorrect(): Bool =
        Assert.eq(List#{7, 15, 30}, parseInputsForTest() |> List.map(r -> r.time))

    @Test
    def parseInputsDistancesAreCorrect(): Bool =
        Assert.eq(List#{9, 40, 200}, parseInputsForTest() |> List.map(r -> r.distance))

    @Test 
    def distance0(): Bool =
        Assert.eq(0, distance(7, 0))

    @Test 
    def distance1(): Bool =
        Assert.eq(6, distance(7, 1))

    @Test 
    def distance3(): Bool =
        Assert.eq(12, distance(7, 3))

    @Test 
    def distance7(): Bool =
        Assert.eq(0, distance(7, 7))

    @Test 
    def winCounts1(): Bool =    
        let race = {time = 7, distance = 9};
        Assert.eq(4, winCounts(race))

    @Test 
    def testPart1(): Bool =
        Assert.eq(288, parseInputsForTest() |> winCountsAllRaces)
}