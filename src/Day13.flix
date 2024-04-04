mod Day13 {
    use Utils.{assertEq, readInputLines, listSplit, Throw}

    pub def part1(): Int32 \ {Throw, IO} = 
        run(readInputLines("Day13"), countReflections)

    pub def run(lines: List[String], part: Vector[Vector[Char]] -> Int32): Int32 =
        def count(g) = {
            let horiz = Grid.rows(g) |> countReflections;
            let cols = Grid.transpose(g) |> Grid.rows;
            let vert = cols |> part;
            100 * horiz + vert
        };
        lines 
            |> listSplit(String.isEmpty)
            |> List.map(Grid.toGrid)
            |> List.sumWith(count) 

    def countReflections(rows: Vector[Vector[Char]]): Int32 =
        def isReflection(i) = {
            let (front, back) = Vector.splitAt(i, rows);
            let frontLen = Vector.length(front);
            let backLen = Vector.length(back);
            let (frontReflection, backReflection) = if (frontLen > backLen) {
                (Vector.takeRight(backLen, front), Vector.reverse(back))
            } else {
                (Vector.reverse(front), Vector.takeLeft(frontLen, back))
            };
            frontReflection == backReflection
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

    @Test 
    pub def testPart1(): Bool \ IO =
        assertEq(405, () -> run(readInputLines("Day13Test"), countReflections))
}