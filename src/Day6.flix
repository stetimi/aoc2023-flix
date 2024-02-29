mod Day6 {
    use Utils.{loop, prod, getOrQuit, getOrThrow, readInputLines, Throw}

    type alias Race = {time = Int64, distance = Int64}

    pub def part1(): Int64 \ {Throw, IO} =
        readInputLines(6)
            |> parseInputs
            |> winCountsAllRaces

    pub def part2(): Int64 =
        let race = {time = 62737565i64, distance = 644102312401023i64};
        winCounts(race)

    def winCountsAllRaces(races: List[Race]): Int64 =
        let counts = List.map(winCounts, races);
        prod(1i64, counts)

    pub def winCounts(race: Race): Int64 =
        let (lowerButtonTime, upperButtonTime) = buttonTimes(race.time, race.distance);
        upperButtonTime - lowerButtonTime - 1i64

    def buttonTimes(raceTime: Int64, distance: Int64): (Int64, Int64) =
        let raceTimeF = Int64.toFloat64(raceTime);
        let distanceF = Int64.toFloat64(distance);
        def sqrt(x) = Float64.pow(x, 0.5);
        let discriminator = sqrt(raceTimeF * raceTimeF - 4.0 * distanceF);
        let lower = (raceTimeF - discriminator) / 2.0;
        let upper = (raceTimeF + discriminator) / 2.0;
        (Float64.tryToInt64(Float64.floor(lower)) |> getOrQuit, Float64.tryToInt64(Float64.ceil(upper)) |> getOrQuit)

    def parseInputs(lines: List[String]): List[Race] =
        let linesVec = List.toVector(lines);
        def parseNumbers(line: String): List[Int64] =
            String.splitOn({substr = " "}, line)
            |> List.map(Int64.parse(10))
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
        Assert.eq(List#{7i64, 15i64, 30i64}, parseInputsForTest() |> List.map(r -> r.time))

    @Test
    def parseInputsDistancesAreCorrect(): Bool =
        Assert.eq(List#{9i64, 40i64, 200i64}, parseInputsForTest() |> List.map(r -> r.distance))
    @Test 
    def winCounts1(): Bool =    
        Assert.eq(4i64, winCounts({time = 7i64, distance = 9i64}))

    @Test 
    def testPart1(): Bool =
        Assert.eq(288i64, parseInputsForTest() |> winCountsAllRaces)
}