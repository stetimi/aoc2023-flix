pub enum Grid[a] {
    case Grid({points = Vector[a], width = Int32})
}

instance ToString[Grid[a]] with ToString[a] {
    pub def toString(grid: Grid[a]): String =
        Grid.rows(grid) |> Vector.map(Vector.join("")) |> Vector.join("\n")
}

mod Grid {
    pub type alias Pos = (Int32, Int32)

    pub def findInGrid(grid: Grid[a], value: a): Option[Pos] with Eq[a] = {
        let Grid(g) = grid;
        def loop(p, i): Option[Pos] = {
            if (p == value) {
                let x = Int32.remainder(i, g.width);
                let y = i / g.width;
                Some((x, y))
            } else if (i == Vector.length(g.points) - 1) {
                None
            } else {
                loop(Vector.get(i + 1, g.points), i + 1)
            }
        };
        loop(Vector.get(0, g.points), 0)
    }

    pub def at(grid: Grid[a], p: Pos): a =
        let (x, y) = p;
        let Grid(g) = grid;
        Vector.get(x + g.width * y, g.points)

    pub def height(grid: Grid[a]): Int32 = 
        let Grid(g) = grid;
        Vector.length(g.points) / g.width

    pub def width(grid: Grid[a]): Int32 = 
        let Grid(g) = grid;
        g.width
        
    pub def within(grid: Grid[a], p: Pos): Bool =
        let (x, y) = p;
        let Grid(g) = grid;
        x >= 0 and y >= 0 and x < g.width and y < height(grid)

    pub def inc(grid: Grid[_], p: Pos): Option[Pos] = 
        let (x, y) = p;
        let Grid({points, width}) = grid;
        let index = y * width + x + 1;
        if (index >= Vector.length(points)) {
            None
        } else {
            let y1 = index / width;
            let x1 = Int32.remainder(index, width);
            Some((x1, y1))
        }

    pub def forEach(f: Pos -> Unit \ e, grid: Grid[_]): Unit \ e =
        def loop(pos) = {
            f(pos);
            match inc(grid, pos) {
                case None => ()
                case Some(pos1) => loop(pos1)
            }
        };
        loop((0, 0))

    pub def filterPositions(f: a -> Bool \ e, grid: Grid[a]): Vector[Pos] \ e = 
        let Grid({points, width}) = grid;
        points |> Vector.findIndices(f) |> Vector.map(toPosW(width))

    pub def toPos(grid: Grid[_], n: Int32): Pos =
        toPosW(width(grid), n)

    pub def toPosW(width: Int32, n: Int32): Pos =
        let y = n / width;
        let x = Int32.remainder(n, width);
        (x, y)

    pub def transpose(grid: Grid[a]): Grid[a] = 
        let Grid({points, width}) = grid;
        let totalPoints = Vector.length(points);
        let height = totalPoints / width;
        def index(i) = {
            let (x, y) = toPos(grid, i);
            x * height + y
        };
        let transposed = region rc {
            let transposedArr = Array.empty(rc, totalPoints);
            Vector.forEachWithIndex((i, x) -> Array.put(x, index(i), transposedArr), points);
            Array.toVector(transposedArr)
        };
        Grid({points=transposed, width=height})

    pub def fromRows(rows: Vector[Vector[a]]): Grid[a] = {
        let width = Vector.get(0, rows) |> Vector.length;
        Grid({points=Vector.flatten(rows), width=width})
    }

    pub def rows(grid: Grid[a]): Vector[Vector[a]] =
        let Grid({points, width}) = grid;
        def loop(ps) = {
            let (first, rest) = Vector.splitAt(width, ps);
            first :: (if (Vector.length(rest) <= width) List#{rest} else loop(rest))
        };
        loop(points) |> List.toVector

    @Test 
    def findInGridIsNoneIfNotThere(): Bool = 
        let g = Grid({points = Vector#{1, 2, 3, 4, 5, 6}, width = 3});
        findInGrid(g, 7) == None

    @Test 
    def findInGridIsValueIfThere(): Bool = 
        let g = Grid({points = Vector#{1, 2, 3, 4, 5, 6}, width = 3});
        findInGrid(g, 4) == Some((0, 1))

    def sample3x2Grid(): Grid[Int32] = 
        Grid({points = Vector#{1, 2, 3, 4, 5, 6}, width = 3})

    @Test 
    def transposedHeightIsOriginalWidth(): Bool = 
        let g = sample3x2Grid() |> transpose;
        Assert.eq(2, width(g))
    
    @Test 
    def transposedPointsAreSwappedCorrectly(): Bool = 
        let Grid({points, width=_}) = sample3x2Grid() |> transpose;
        Assert.eq(Vector#{1, 4, 2, 5, 3, 6}, points)

    @Test 
    pub def rowsWorks(): Bool = {
        sample3x2Grid() 
            |> rows
            |> Assert.eq(Vector#{Vector#{1,2,3}, Vector#{4,5,6}})
    }

    @Test 
    def stringRep(): Bool =
        let s = ToString.toString(sample3x2Grid());
        Assert.eq("123\n456", s)
}
