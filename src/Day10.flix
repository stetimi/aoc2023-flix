mod Day10 {
    use Utils.{assertEq, getOrThrow, headOrThrow, parseInt32OrThrow, readInputLines, Throw}
    use Grid.{Grid, Pos, findInGrid}

    enum Direction with Eq, Order, ToString {case N, E, S, W} 

    pub def part1(): Int32 \ {IO, Throw} = 
        run(readInputLines("Day10") |> List.toVector, findStepsToFarthestPoint)
    
    pub def run(lines: Vector[String], part: Grid[Char] -> Int32 \ Throw): Int32 \ Throw =
        let points = lines
            |> Vector.map(String.toVector)
            |> Vector.flatten;
        let width = Vector.get(0, lines) |> String.length;
        let grid = Grid.Grid({points = points, width = width});
        part(grid)

    def findStepsToFarthestPoint(grid: Grid[Char]): Int32 \ Throw = {
        let start = findInGrid(grid, 'S') |> getOrThrow;
        def next(pos, d) = {
            let ch = Grid.at(grid, pos);
            forM (
                d1 <- nextDirection(ch, d);
                p1 <- move(grid, pos, d1)
            ) yield (p1, d1)
        };
        def loop(len, pos, d) = {
            if (len > 0 and pos == start) {
                len
            } else {
              let opd = next(pos, d);
              match opd {
                  case None => len
                  case Some(pd) => {
                    let (p1, d1) = pd;
                    loop(len + 1, p1, d1)
                  }
              }
            }
        };
        Vector#{Direction.S, Direction.N, Direction.W, Direction.E} 
            |> Vector.map(loop(0, start))
            |> Vector.maximum
            |> Option.map(l -> l / 2)
            |> getOrThrow
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
        let simple = Vector#{
            "-L|F7",
            "7S-7|",
            "L|7||",
            "-L-J|",
            "L|-JF"
        };
        assertEq(4, () -> run(simple, findStepsToFarthestPoint))
}