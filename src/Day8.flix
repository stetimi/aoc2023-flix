mod Day8 {
    use Utils.{assertEq, getOrThrow, Throw, readInputLines}
    type alias Input = {instructions = List[Char], mappings = Map[String, {left = String, right = String}]}

    pub def run(filename: String, part: Input -> Int64 \ Throw): Int64 \ {IO, Throw} =
        readInputLines(filename)
            |> List.toVector
            |> parseInput
            |> part

    def parseInput(lines: Vector[String]): Input =
        def split(s) = 
            (String.takeLeft(3, s), {left = String.slice({start=7}, {end=10}, s), right = String.slice({start=12}, {end=15}, s)});
        let instructions = Vector.get(0, lines)
            |> String.toList;
        let mappings = Vector.dropLeft(2, lines)
            |> Vector.map(split)
            |> Vector.toMap;
        {instructions = instructions, mappings = mappings}

    pub def part1(input: Input): Int64 \ Throw =
        followInstructions(0i64, input, n -> n == "ZZZ", "AAA")
    
    pub def part2(input: Input): Int64 \ Throw =
        Map.keysOf(input.mappings)
            |> Set.filter(String.endsWith({suffix="A"}))
            |> Set.toList
            |> List.map(followInstructions(0i64, input, String.endsWith({suffix="Z"})))
            |> List.foldLeft(lcm, 1i64)

    def lcm(a: Int64, b: Int64): Int64 =
        a * b / gcd(a, b)

    def gcd(a: Int64, b: Int64): Int64 = 
        if (b == 0i64) a else gcd(b, Int64.remainder(a, b))

    def followInstructions(count: Int64, input: Input, atDest: String -> Bool, start: String): Int64 \ Throw =
        def loop(loopCount, current, instructions): Int64 \ Throw = {
            match instructions {
                case Nil => 
                    if (atDest(current)) {
                        loopCount
                    } else {
                        loop(loopCount, current, input.instructions)
                    }
                case Cons(i, rest) =>
                    let {left = left, right = right} = Map.get(current, input.mappings) |> getOrThrow;
                    let next = match i {
                        case 'L' => left
                        case 'R' => right
                        case _ => bug!("Unhandled ${i}")
                    };
                    if (atDest(next)) {
                        loopCount + 1i64
                    } else {
                        loop(loopCount + 1i64, next, rest)                        
                    }
            }
        };
        loop(count, start, input.instructions)

    def sampleInput1(): Vector[String] =
        Vector#{
            "LLR",
            "",
            "AAA = (BBB, BBB)",
            "BBB = (AAA, ZZZ)",
            "ZZZ = (ZZZ, ZZZ)"
        }

    def sampleInput2(): Vector[String] = 
        Vector#{
            "RL",
            "",
            "AAA = (BBB, CCC)",
            "BBB = (DDD, EEE)",
            "CCC = (ZZZ, GGG)",
            "DDD = (DDD, DDD)",
            "EEE = (EEE, EEE)",
            "GGG = (GGG, GGG)",
            "ZZZ = (ZZZ, ZZZ)"
        }

    @Test 
    pub def testParseInputInstructions(): Bool =
        let input = parseInput(sampleInput1());
        Assert.eq("LLR" |> String.toList, input.instructions)

    
    @Test 
    pub def testParseInputMappings(): Bool =
        let input = parseInput(sampleInput1());
        Assert.eq(Set#{"AAA", "BBB", "ZZZ"}, input.mappings |> Map.keysOf)
        and Assert.eq(List#{"BBB", "AAA", "ZZZ"}, input.mappings |> Map.valuesOf |> List.map(x -> x.left))
        and Assert.eq(List#{"BBB", "ZZZ", "ZZZ"}, input.mappings |> Map.valuesOf |> List.map(x -> x.right))

    @Test 
    pub def testPart1Sample1(): Bool =
        assertEq(6i64, () ->
            sampleInput1()
                |> parseInput
                |> part1)

    @Test 
    pub def testPart1Sample2(): Bool =
        assertEq(2i64, () -> 
            sampleInput2()
                |> parseInput
                |> part1)

    pub def testPart2(): Bool =
        assertEq(6i64, () -> run("Day8Test", part2))
}