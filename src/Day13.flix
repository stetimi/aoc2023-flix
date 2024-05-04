mod Day13 {
    use Utils.{assertEq, readInputLines, listSplit, Throw}

    pub def part1(): Int32 \ {Throw, IO} = 
        run(readInputLines("Day13"), countReflections(0))

    pub def part2(): Int32 \ {Throw, IO} = 
        run(readInputLines("Day13"), countReflections(1))

    pub def run(lines: List[String], part: Vector[Vector[Char]] -> Int32): Int32 =
        def count(g) = {
            let horiz = Grid.rows(g) |> part;
            let cols = Grid.transpose(g) |> Grid.rows;
            let vert = cols |> part;
            100 * horiz + vert
        };
        lines 
            |> listSplit(String.isEmpty)
            |> List.map(Grid.toGrid)
            |> List.sumWith(count) 

    def countReflections(smudges: Int32, rows: Vector[Vector[Char]]): Int32 =
        def isReflection(i) = {
            let (front, back) = Vector.splitAt(i, rows);
            let reflectionLen = Int32.min(Vector.length(front), Vector.length(back));
            let frontReflection = Vector.takeRight(reflectionLen, front);
            let backReflection = Vector.takeLeft(reflectionLen, back) |> Vector.reverse;
            hasDiffs(frontReflection, backReflection, smudges)
        };
        def loop(i) = {
            if (i >= Vector.length(rows)) {
                None
            } else if (isReflection(i)) {
                Some(i)
            } else {
                loop(i + 1)
            }
        };
        loop(1) |> Option.getWithDefault(0)

    pub def hasDiffs(fs: Vector[Vector[Char]], bs: Vector[Vector[Char]], diffs: Int32): Bool =
        let size = Vector.length(fs);
        let fbs = fs `Vector.zip` bs;
        def countDiffs(f, b) = {
            let fb = f `Vector.zip` b;
            Vector.count(match (x, y) -> x != y, fb)
        };
        def loop(i, count) =
            if (count > diffs or i == size) {
                count
            } else {
                let (f, b) = Vector.get(i, fbs);
                let newCount = count + countDiffs(f, b);
                loop(i + 1, newCount)
            };
        let total = loop(0, 0);
        total == diffs

    @Test 
    pub def testPart1(): Bool =
        assertEq(405, () -> run(readInputLines("Day13Test"), countReflections(0)))

    @Test 
    pub def testPart2(): Bool =
        assertEq(400, () -> run(readInputLines("Day13Test"), countReflections(1)))

    @Test 
    pub def hasDiffsWith2DiffsNegative(): Bool =
        let f = Vector#{"##..##", "#####.", "...#.."} |> Vector.map(String.toVector);
        let b = Vector#{"##..#.", "#####.", "#..#.."} |> Vector.map(String.toVector);
        not hasDiffs(f, b, 0) and not hasDiffs(f, b, 1) and not hasDiffs(f, b, 3)

    @Test 
    pub def hasDiffsWith2DiffsPositive(): Bool =
        let f = Vector#{"##..##", "#####.", "...#.."} |> Vector.map(String.toVector);
        let b = Vector#{"##..#.", "#####.", "#..#.."} |> Vector.map(String.toVector);
        hasDiffs(f, b, 2)

    @Test 
    pub def hasDiffsWith0Diffs(): Bool =
        let f = Vector#{"##..##", "#####.", "...#.."} |> Vector.map(String.toVector);
        hasDiffs(f, f, 0) and not hasDiffs(f, f, 1)
}