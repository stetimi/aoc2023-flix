mod Day8 {
    use Utils.{getOrQuit, uncheckedReadInputLines}
    type alias Input = {instructions = List[Char], mappings = Map[String, {left = String, right = String}]}

    pub def part1(): Int32 =
        uncheckedReadInputLines("Day8")
            |> List.toVector
            |> parseInput
            |> run

    def parseInput(lines: Vector[String]): Input =
        def split(s) = 
            (String.takeLeft(3, s), {left = String.slice({start=7}, {end=10}, s), right = String.slice({start=12}, {end=15}, s)});
        let instructions = Vector.get(0, lines)
            |> String.toList;
        let mappings = Vector.dropLeft(2, lines)
            |> Vector.map(split)
            |> Vector.toMap;
        {instructions = instructions, mappings = mappings}

    def run(input: Input): Int32 =
        followInstructions(0, "AAA", input)

    def followInstructions(count: Int32, start: String, input: Input): Int32 =
        def loop(loopCount, current, instructions) = {
            match instructions {
                case Nil => 
                    if (current == "ZZZ") {
                        (loopCount, current)
                    } else {
                        loop(loopCount, current, input.instructions)
                    }
                case Cons(i, rest) =>
                    let choices = Map.get(current, input.mappings) |> getOrQuit;
                    let next = match i {
                        case 'L' => choices.left
                        case 'R' => choices.right
                        case _ => bug!("Unhandled ${i}")
                    };
                    if (next == "ZZZ") {
                        (loopCount + 1, "ZZZ")
                    } else {
                        loop(loopCount + 1, next, rest)                        
                    }
            }
        };
        loop(count, start, input.instructions) |> fst

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
        let ans = sampleInput1()
            |> parseInput
            |> run;
        Assert.eq(6, ans)

    @Test 
    pub def testPart1Sample2(): Bool =
        let ans = sampleInput2()
            |> parseInput
            |> run;
        Assert.eq(2, ans)
}