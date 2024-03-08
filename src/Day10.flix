mod Day10 {
    use Utils.{assertEq, getOrThrow, headOrThrow, parseInt32OrThrow, readInputLines, Throw}
    use Grid.{Grid, Pos, findInGrid}

    enum Direction with Eq, Order {case N, E, S, W} 

    pub def part1(): Int32 \ {IO, Throw} = 
        run("Day10", findStepsToFarthestPoint)
    
    pub def run(file: String, part: Grid[Char] -> Int32 \ Throw): Int32 \ {Throw, IO} =
        let lines = readInputLines(file)
            |> List.toVector;
        let points = lines
            |> Vector.map(String.toVector)
            |> Vector.flatten;
        let width = Vector.get(0, lines) |> String.length;
        let grid = Grid.Grid({points = points, width = width});
        part(grid)

    def findStepsToFarthestPoint(grid: Grid[Char]): Int32 \ Throw = region rc {
        let start = findInGrid(grid, 'S') |> getOrThrow;
        let directions = Vector#{ Direction.N, Direction.E, Direction.S, Direction.W };
        let visited = MutSet.new(rc);
        def alreadyVisited(p: Pos) = 
            MutSet.exists(q -> q == p, visited);
        def loop(pos_s, steps) = {
            let _x: List[Direction] = foreach (d <- directions; pos <- pos_s) 
                yield d;
            
            //     let next = move(grid, pos, d);
            //     yield (next);
            // };
            // let nexts = directions
            //     |> Vector.filterMap(move(grid, start))
            //     |> Vector.filter(p -> MutSet.exists(q -> q == p, visited));
            // if (Vector.isEmpty(nexts)) {
            //     steps
            // } else {
            //     MutSet.addAll!(nexts, visited);
            //     loop(nexts, steps + 1)
            // }
            ?a
        };
        loop(Vector#{start}, 0)
    }

    pub def move(g: Grid[Char], p: Pos, d: Direction): Option[Pos] = 
        let ch = Grid.at(g, p);
        def mkPos(x, y): Option[Pos] = if (Grid.within(g, (x, y))) {
            Some((x, y))
        } else {
            None
        };
        let (x, y) = p;
        match (d, ch) {
            case (Direction.N, '|') => mkPos(x, y - 1)
            case (Direction.S, '|') => mkPos(x, y + 1)
            case (Direction.E, '-') => mkPos(x + 1, y)
            case (Direction.W, '-') => mkPos(x - 1, y)
            case _ => None
        }

    @Test 
    pub def x(): Bool = 
        true
}