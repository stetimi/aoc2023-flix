mod Day12 {
    use Utils.{assertEq, readInputLines, parseInt32OrThrow, Throw}

    type alias ParsedLine = {springs = Vector[Char], criteria = Vector[Int32]}

    pub def part1(): Int32 \ {IO, Throw} = 
        run(readInputLines("Day12") |> List.map(toRecord), countConditionRecords)

    pub def run(lines: List[ParsedLine], part: ParsedLine -> Int32 \ e): Int32 \ e =
        List.sumWith(part, lines)

    def toRecord(line: String): ParsedLine \ Throw =
        let split = String.splitOn(substr=" ", line) |> List.toVector;
        let springs = Vector.get(0, split) |> String.toVector;
        let criteriaString = Vector.get(1, split);
        let criteria = String.splitOn(substr=",", criteriaString)
            |> List.map(parseInt32OrThrow)
            |> List.toVector;
        {springs=springs, criteria=criteria}
        
    def countConditionRecords(line: ParsedLine): Int32 = 
        generateRecords(line.springs, 0)
            |> Vector.map(matchesCriteria(line.criteria))
            |> Vector.count(x -> x)

    def generateRecords(record: Vector[Char], index: Int32): Vector[Vector[Char]] =
        if (index >= Vector.length(record)) {
            Vector#{record}
        } else {
            if (Vector.get(index, record) == '?') {
                let undamaged = Vector.update(index, '.', record);
                let damaged = Vector.update(index, '#', record);
                generateRecords(undamaged, index + 1) ++ generateRecords(damaged, index + 1)
            } else {
                generateRecords(record, index + 1)
            }
        }

    def matchesCriteria(criteria: Vector[Int32], record: Vector[Char]): Bool = region rc {
        let counts = MutList.empty(rc);
        let rcount = ref 0 @ rc;
        record |> Vector.forEach(ch -> {
            let count = deref rcount;
            match ch {
                case '#' => rcount := count + 1
                case '.' => {
                    if (count != 0) {
                        MutList.push!(count, counts);
                        rcount := 0
                    } else {
                        ()
                    }
                }
                case _ => unreachable!()
            }
        });
        if (deref rcount != 0) {
            MutList.push!(deref rcount, counts)
        } else ();
        let countsVector = MutList.toVector(counts);
        criteria == countsVector
    }

    @Test 
    def testPart1(): Bool \ IO =
        assertEq(21, () -> run(readInputLines("Day12Test") |> List.map(toRecord), countConditionRecords))

    @Test 
    pub def testGenerateRecords(): Bool =
        let s = "???.###" |> String.toVector;
        let combos = generateRecords(s, 0) |> Vector.map(Vector.join(""));
        Assert.eq(Vector#{"....###", "..#.###", ".#..###", ".##.###", "#...###", "#.#.###", "##..###", "###.###"}, combos)

    @Test 
    pub def matchesCriteriaWhenMatchesAndEndsOnHash(): Bool =
        let record = "##..###.#" |> String.toVector;
        let criteria = Vector#{2, 3, 1};
        matchesCriteria(criteria, record)

    @Test 
    pub def matchesCriteriaWhenMatchesAndDoesNotEndOnHash(): Bool =
        let record = "##..###.." |> String.toVector;
        let criteria = Vector#{2, 3};
        matchesCriteria(criteria, record)

    @Test 
    pub def matchesCriteriaWhenNoMatch(): Bool =
        let record = "##..###.#" |> String.toVector;
        let criteria = Vector#{2, 3, 2};
        not matchesCriteria(criteria, record)    
}