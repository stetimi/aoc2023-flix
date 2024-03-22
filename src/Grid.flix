pub enum Grid[a] {
    case Grid({points = Vector[a], width = Int32})
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

    pub def count(pred: Pos -> Bool \ e, grid: Grid[_]): Int32 \ e = region rc {
        let rcount = ref 0 @ rc;
        def f(pos) = if (pred(pos)) {
            rcount := deref rcount + 1;
            ()
        } else {
            ()
        };
        forEach(f, grid);
        deref rcount
    }

    pub def toPos(grid: Grid[_], n: Int32): Pos =
        let width = width(grid);
        let y = n / width;
        let x = Int32.remainder(n, width);
        (x, y)

    @Test 
    def findInGridIsNoneIfNotThere(): Bool = {
        let g = Grid({points = Vector#{1, 2, 3, 4, 5, 6}, width = 3});
        findInGrid(g, 7) == None
    }

    @Test 
    def findInGridIsValueIfThere(): Bool = {
        let g = Grid({points = Vector#{1, 2, 3, 4, 5, 6}, width = 3});
        findInGrid(g, 4) == Some((0, 1))
    }
}