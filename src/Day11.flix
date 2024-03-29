mod Day11 {
    use Utils.{assertEq, readInputLines, Throw}
    use Grid.{Pos};

    pub def part1(): Int32 \ {IO, Throw} = 
        run(readInputLines("Day11") |> List.toVector, sumShortestPaths)
    
    pub def run(lines: Vector[String], part: Grid[Char] -> Int32 \ e): Int32 \ e =
        let points = lines
            |> Vector.map(String.toVector)
            |> Vector.flatten;
        let width = Vector.get(0, lines) |> String.length;
        let grid = Grid.Grid({points = points, width = width});
        part(grid)

    def sumShortestPaths(grid: Grid[Char]): Int32 =
        let emptyRowIndices = findEmptyRowIndices(grid);
        let emptyColumnIndices = findEmptyRowIndices(grid |> Grid.transpose);
        let galaxyPositions = Grid.filterPositions(ch -> ch == '#', grid);
        let galaxyPairs = allPairs(galaxyPositions |> Vector.toList);
        def shortest(p1, p2) = {
            let (x1, y1) = p1;
            let (x2, y2) = p2;
            let colDiff = spaceBetween(emptyColumnIndices, x1, x2);
            let rowDiff = spaceBetween(emptyRowIndices, y1, y2);
            colDiff + rowDiff
        };
        galaxyPairs |> List.sumWith(match (p1, p2) -> shortest(p1, p2))

    def findEmptyRowIndices(grid: Grid[Char]): Vector[Int32] =
        let rows = Grid.rows(grid);
        def isEmptyRow(r) = Vector.forAll(ch -> ch == '.', r);
        rows 
            |> Vector.mapWithIndex((i, r) -> if (isEmptyRow(r)) Some(i) else None)
            |> Vector.filterMap(x -> x)

    def spaceBetween(emptyIndices: Vector[Int32], p1: Int32, p2: Int32): Int32 =
        if (p2 < p1) {
            spaceBetween(emptyIndices, p2, p1)
        } else {
            def indexWithinPoints(ep) = ep > p1 and ep < p2;
            let numExpanded = forA(
                firstAfterP1 <- Vector.findIndexOfRight(indexWithinPoints, emptyIndices);
                firstBeforeP2 <- Vector.findIndexOf(indexWithinPoints, emptyIndices)
            ) yield firstAfterP1 - firstBeforeP2 + 1;
            (p2 - p1) + numExpanded |> Option.getWithDefault(0)
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
        assertEq(374, () -> run("Day11Test" |> readInputLines |> List.toVector, sumShortestPaths))
}