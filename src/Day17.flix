mod Day17 {
    use Utils.{assertEq, forEachInRange, getOrThrow, parseInt32OrThrow, readInputLines, listSplit, listSum, Throw}
    use Grid.{Grid, Direction, Pos, turnLeft, turnRight}

    enum MoveTracking(Pos, Direction, Int32) with Eq, ToString, Order

    pub def part1(): Int32 \ {Throw, IO} = 
        run("Day17", findPart1MinHeatLossPath)

    pub def run(file: String, part: Grid[Int32] -> Int32 \ Throw): Int32 \ {Throw, IO} =
        readInputLines(file) 
            |> Grid.toGrid 
            |> Functor.map(Char.digit(radix=10) >> getOrThrow)
            |> part

    pub def findPart1MinHeatLossPath(grid: Grid[Int32]): Int32 \ Throw = region rc {
        let dest = Grid.bottomRightPos(grid);
        let seen = MutSet.empty(rc);
        let pq = PriorityQueue.emptyWithComparison(rc, (x1, x2) -> Order.compare(x1.heatLoss, x2.heatLoss));
        def insertStart!(d) = {
            let mt = MoveTracking.MoveTracking((0, 0), d, 0);
            PriorityQueue.insert!(rc, {mt=mt, heatLoss=0}, pq);
            MutSet.add!(mt, seen)
        };
        insertStart!(Direction.E);
        insertStart!(Direction.S);
        def loop() = {
            let {mt, heatLoss} = PriorityQueue.pop!(pq) |> getOrThrow;
            let MoveTracking.MoveTracking(pos, _, _) = mt;
            if (pos == dest) {
                heatLoss
            } else {
                let nextMts = moves(grid, mt)
                    |> List.filter(mt1 -> not MutSet.memberOf(mt1, seen));
                foreach (nextMt <- nextMts) {
                    let MoveTracking.MoveTracking(pos1, _, _) = nextMt;
                    let nextHeatLoss = heatLoss + Grid.at(grid, pos1);
                    PriorityQueue.insert!(rc, {mt=nextMt, heatLoss=nextHeatLoss}, pq);
                    MutSet.add!(nextMt, seen)
                };
                loop()
            }
        };
        loop()
    }
 
    def moves(grid: Grid[_], mt: MoveTracking): List[MoveTracking] = region rc {
        let MoveTracking.MoveTracking(pos, direction, steps) = mt;
        let nextMoves = MutList.empty(rc);
        def push!(d, steps1) = {
            match Grid.move(pos, d, grid) {
                case None => ()
                case Some(pos1) => MutList.push!(MoveTracking.MoveTracking(pos1, d, steps1), nextMoves)
            }
        };
        push!(turnLeft(direction), 1);
        push!(turnRight(direction), 1);
        if (steps < 3) {
            push!(direction, steps + 1)
        } else ();
        MutList.toList(nextMoves)
    }

    @Test 
    pub def testPart1(): Bool \ IO = {
        assertEq(102, () -> run("Day17Test", findPart1MinHeatLossPath))
    }
}
