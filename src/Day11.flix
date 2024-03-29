mod Day11 {
    use Utils.{assertEq, readInputLines, Throw}
    use Grid.{Pos};

    pub def part1(): Int64 \ {IO, Throw} = 
        run(readInputLines("Day11") |> List.toVector, sumShortestPaths(2))
    
    pub def part2(): Int64 \ {IO, Throw} = 
        run(readInputLines("Day11") |> List.toVector, sumShortestPaths(1_000_000))
    
    pub def run(lines: Vector[String], part: Grid[Char] -> Int64 \ e): Int64 \ e =
        let points = lines
            |> Vector.map(String.toVector)
            |> Vector.flatten;
        let width = Vector.get(0, lines) |> String.length;
        let grid = Grid.Grid({points = points, width = width});
        part(grid)

    def sumShortestPaths(expansionFactor: Int32, grid: Grid[Char]): Int64 =
        let emptyRowIndices = findEmptyRowIndices(grid);
        let emptyColumnIndices = findEmptyRowIndices(grid |> Grid.transpose);
        let galaxyPositions = Grid.filterPositions(ch -> ch == '#', grid);
        let galaxyPairs = allPairs(galaxyPositions |> Vector.toList);
        def shortest(p1, p2): Int32 = {
            let (x1, y1) = p1;
            let (x2, y2) = p2;
            let colDiff = spaceBetween(emptyColumnIndices, expansionFactor, x1, x2);
            let rowDiff = spaceBetween(emptyRowIndices, expansionFactor, y1, y2);
            colDiff + rowDiff
        };
        region rc {
            let sum = ref 0i64 @ rc;
            galaxyPairs |> List.forEach(match (p1, p2) -> sum := deref sum + shortest(p1, p2) |> Int32.toInt64);
            deref sum
        }

    def findEmptyRowIndices(grid: Grid[Char]): Vector[Int32] =
        let rows = Grid.rows(grid);
        def isEmptyRow(r) = Vector.forAll(ch -> ch == '.', r);
        rows 
            |> Vector.mapWithIndex((i, r) -> if (isEmptyRow(r)) Some(i) else None)
            |> Vector.filterMap(x -> x)

    def spaceBetween(emptyIndices: Vector[Int32], expansionFactor: Int32, p1: Int32, p2: Int32): Int32 =
        if (p2 < p1) {
            spaceBetween(emptyIndices, expansionFactor, p2, p1)
        } else {
            def loop(n, count) = {
                if (n >= Vector.length(emptyIndices)) {
                    count
                } else {
                    let emptyIndex = Vector.get(n, emptyIndices);
                    if (emptyIndex > p1 and emptyIndex < p2) {
                        loop(n + 1, count + expansionFactor - 1)
                    } else {
                        loop(n + 1, count)
                    }
                }
            };
            (p2 - p1) + loop(0, 0)
        }
    
    def allPairs(ps: List[a]): List[(a, a)] =
        match ps {
            case Nil => Nil
            case Cons(h, t) => {
                let hPairs = List.map(t1 -> (h, t1), t);
                let tPairs = allPairs(t);
                hPairs ++ tPairs
            }
        }

    @Test 
    pub def part1Test(): Bool \ IO =
        assertEq(374i64, () -> run("Day11Test" |> readInputLines |> List.toVector, sumShortestPaths(2)))

    @Test 
    pub def part2Test(): Bool \ IO =
        assertEq(1030i64, () -> run("Day11Test" |> readInputLines |> List.toVector, sumShortestPaths(10)))
}