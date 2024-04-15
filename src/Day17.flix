mod Day17 {
    use Utils.{assertEq, forEachInRange, getOrThrow, parseInt32OrThrow, readInputLines, listSplit, listSum, Throw}
    use Grid.{Grid, Direction, Pos, turnLeft, turnRight}

    enum MoveTracking(Pos, Direction, Int32) with Eq, ToString

    def mtPos(mt: MoveTracking): Pos =
        let MoveTracking.MoveTracking(pos, _, _) = mt;
        pos

    pub def part1(): Int32 \ {Throw, IO} = 
        run("Day17", findMinHeatLossPath)

    pub def run(file: String, part: Grid[Int32] -> Int32 \ Throw): Int32 \ {Throw, IO} =
        readInputLines(file) 
            |> Grid.toGrid 
            |> Functor.map(Char.digit(radix=10) >> getOrThrow)
            |> part

    pub def findMinHeatLossPath(grid: Grid[Int32]): Int32 \ Throw = region rc {
        let dest = Grid.bottomRightPos(grid);
        let seen = MutSet.empty(rc);
        let pq = PriorityQueue.emptyWithComparison(rc, (x1, x2) -> Order.compare(x1.heatLoss, x2.heatLoss));
        def insertStart!(d) = {
            let mt = MoveTracking.MoveTracking((0, 0), d, 0);
            PriorityQueue.insert!(rc, {mt=mt, path=List#{((0,0),0)}, heatLoss=0}, pq)
        };
        insertStart!(Direction.S);
        insertStart!(Direction.E);
        def loop() = {
            let {mt, path, heatLoss} = PriorityQueue.pop!(pq) |> getOrThrow;
            let MoveTracking.MoveTracking(pos, _, _) = mt;
            if (pos == dest) {
                (path, heatLoss)
            } else {
                let nextMts = moves(grid, mt)
                    |> List.filter(mt1 -> not MutSet.memberOf(mtPos(mt1), seen));
                foreach (nextMt <- nextMts) {
                    let MoveTracking.MoveTracking(pos1, _, _) = nextMt;
                    let nextHeatLoss = heatLoss + Grid.at(grid, pos1);
                    PriorityQueue.insert!(rc, {mt=nextMt, path=(pos1,nextHeatLoss)::path, heatLoss=nextHeatLoss}, pq);
                    MutSet.add!(pos1, seen)
                };
                loop()
            }
        };
        let (path, heatLoss) = loop();
        debug(path |> List.reverse |> List.join(","));
        heatLoss
    }
 
    def moves(grid: Grid[_], mt: MoveTracking): List[MoveTracking] = {
        let MoveTracking.MoveTracking(pos, direction, steps) = mt;
        def nextMoveTracking(d) = {
            let nextSteps = if (d == direction) steps + 1 else 1;
             Grid.move(pos, d, grid) |> Option.map(p -> MoveTracking.MoveTracking(p, d, nextSteps))
        };
        let possibleDirections = if (steps < 3) {
            List#{turnLeft(direction), direction, Grid.turnRight(direction)}
        } else {
            List#{turnLeft(direction), Grid.turnRight(direction)}
        };
        possibleDirections |> List.filterMap(nextMoveTracking)
    }

    @Test 
    pub def testPart1(): Bool \ IO = {
        assertEq(102, () -> run("Day17Test", findMinHeatLossPath))
    }
}
