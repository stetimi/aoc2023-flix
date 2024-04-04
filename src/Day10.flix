mod Day10 {
    use Utils.{assertEq, comparing, getOrThrow, headOrThrow, parseInt32OrThrow, readInputLines, Throw}
    use Grid.{Grid, Pos, findInGrid}

    enum Direction with Eq, Order, ToString {case N, E, S, W}

    pub def part1(): Int32 \ {IO, Throw} = 
        run(readInputLines("Day10"), countStepsToFarthestPoint)
    
    pub def part2(): Int32 \ {IO, Throw} = 
        run(readInputLines("Day10"), countTilesWithinGrid)

    pub def run(lines: List[String], part: Grid[Char] -> Int32 \ e): Int32 \ e =
        lines |> Grid.toGrid |> part

    def countStepsToFarthestPoint(grid: Grid[Char]): Int32 \ Throw = 
        let count = grid |> findStepsToFarthestPoint |> List.length;
        count / 2
    
    def findStepsToFarthestPoint(grid: Grid[Char]): List[Pos] \ Throw = {
        let start = findInGrid(grid, 'S') |> getOrThrow;
        def next(pos, d) = {
            let ch = Grid.at(grid, pos);
            forM (
                d1 <- nextDirection(ch, d);
                p1 <- move(grid, pos, d1)
            ) yield (p1, d1)
        };
        def loop(steps, pos, d) = {
            if (List.nonEmpty(steps) and pos == start) {
                steps
            } else {
              let opd = next(pos, d);
              match opd {
                  case None => steps
                  case Some(pd) => {
                    let (p1, d1) = pd;
                    loop(p1 :: steps, p1, d1)
                  }
              }
            }
        };
        Vector#{Direction.S, Direction.N, Direction.W, Direction.E} 
            |> Vector.map(loop(Nil, start))
            |> Vector.maximumBy(comparing(List.length))
            |> getOrThrow
    }

    ///
    /// Uses Shoelace theorem and Pick's Theorem
    ///
    def countTilesWithinGrid(grid: Grid[Char]): Int32 \ Throw = {
        let boundary = findStepsToFarthestPoint(grid) |> List.toVector;
        let n = Vector.length(boundary);
        // Shoelace formula, calculating the area within the boundary.
        def loop(area, i) =
            if (i == n) {
                area
            } else {
                let j = Int32.remainder(i + 1, n);
                let (ix, iy) = Vector.get(i, boundary);
                let (jx, jy) = Vector.get(j, boundary);
                loop(area + (ix * jy) - (jx * iy), i + 1)
            };
        let area = loop(0, 0);
        // Pick's theorem, to count the number of points within the polygon.
        Int32.abs(area) / 2 - n / 2 + 1
    }
        
    pub def nextDirection(ch: Char, d: Direction): Option[Direction] = 
        match (d, ch) {
            case (Direction.N, '|') => Some(d)
            case (Direction.S, '|') => Some(d)
            case (Direction.E, '-') => Some(d)
            case (Direction.W, '-') => Some(d)
            case (Direction.S, 'L') => Some(Direction.E)
            case (Direction.W, 'L') => Some(Direction.N)
            case (Direction.E, 'J') => Some(Direction.N)
            case (Direction.S, 'J') => Some(Direction.W)
            case (Direction.E, '7') => Some(Direction.S)
            case (Direction.N, '7') => Some(Direction.W)
            case (Direction.W, 'F') => Some(Direction.S)
            case (Direction.N, 'F') => Some(Direction.E)
            case (_, 'S') => Some(d)
            case _ => None
        }

    def move(g: Grid[Char], p: Pos, d: Direction): Option[Pos] =
        let (x, y) = p;
        let next = match d {
            case Direction.N => (x, y - 1)
            case Direction.S => (x, y + 1)
            case Direction.W => (x - 1, y)
            case Direction.E => (x + 1, y)
        };
        if (Grid.within(g, next)) Some(next) else None

    @Test 
    pub def part1Test(): Bool = 
        let simple = List#{
            "-L|F7",
            "7S-7|",
            "L|7||",
            "-L-J|",
            "L|-JF"
        };
        assertEq(4, () -> run(simple, countStepsToFarthestPoint))

    @Test
    pub def part2Simple(): Bool =
        let grid = List#{
        //   01234567890
            "...........", // 0
            ".S-------7.", // 1
            ".|F-----7|.", // 2
            ".||.....||.", // 3
            ".||.....||.", // 4
            ".|L-7.F-J|.", // 5
            ".|..|.|..|.", // 6
            ".L--J.L--J.", // 7
            "..........."
        };
        assertEq(4, () -> run(grid, countTilesWithinGrid))

    @Test
    pub def part2LargerExample(): Bool = 
        let grid = List#{
         //  01234567890123456789   
            ".F----7F7F7F7F-7....",
            ".|F--7||||||||FJ....",
            ".||.FJ||||||||L7....",
            "FJL7L7LJLJ||LJ.L-7..",
            "L--J.L7...LJS7F-7L7.",
            "....F-J..F7FJ|L7L7L7",
            "....L7.F7||L7|.L7L7|",
            ".....|FJLJ|FJ|F7|.LJ",
            "....FJL-7.||.||||...",
            "....L---J.LJ.LJLJ..."
        };
        assertEq(8, () -> run(grid, countTilesWithinGrid))
}