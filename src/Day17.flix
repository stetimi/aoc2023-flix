mod Day17 {
    use Utils.{assertEq, forEachInRange, getOrThrow, parseInt32OrThrow, readInputLines, listSplit, listSum, Throw}
    use Grid.{Grid, Direction, Pos, turnLeft, turnRight}

    enum MoveTracking(Pos, Direction, Int32) with Eq, ToString, Order

    pub def part1(): Int32 \ {Throw, IO} = 
        runPart1("Day17")

    pub def part2(): Int32 \ {Throw, IO} = 
        runPart2("Day17")

    pub def runPart1(file: String): Int32 \ {Throw, IO} =
        let grid = readInputLines(file) 
            |> Grid.toGrid 
            |> Functor.map(Char.digit(radix=10) >> getOrThrow);
        def isValid(direction, steps, nextDirection) = {
            steps < 3 or direction != nextDirection
        };
        findMinHeatLossPath(1, moves(grid, isValid), grid)

    pub def runPart2(file: String): Int32 \ {Throw, IO} =
        let grid = readInputLines(file) 
            |> Grid.toGrid 
            |> Functor.map(Char.digit(radix=10) >> getOrThrow);
        def isValid(direction, steps, nextDirection) = {
            if (steps >= 10) {
                direction != nextDirection
            } else if (steps < 4) {
                direction == nextDirection
            } else true
        };
        findMinHeatLossPath(4, moves(grid, isValid), grid)

    pub def findMinHeatLossPath(minSteps: Int32, nextMoves: MoveTracking -> List[MoveTracking], grid: Grid[Int32]): Int32 \ Throw = region rc {
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
            let MoveTracking.MoveTracking(pos, _, steps) = mt;
            if (pos == dest and steps >= minSteps) {
                heatLoss
            } else {
                let nextMts = nextMoves(mt)
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
 
    def moves(grid: Grid[_], isValid: (Direction, Int32, Direction) -> Bool, mt: MoveTracking): List[MoveTracking] = region rc {
        let MoveTracking.MoveTracking(pos, direction, steps) = mt;
        let nextMoves = MutList.empty(rc);
        def push!(d, steps1) = {
            match Grid.move(pos, d, grid) {
                case None => ()
                case Some(pos1) => 
                    if (isValid(direction, steps, d)) {
                        MutList.push!(MoveTracking.MoveTracking(pos1, d, steps1), nextMoves)
                    } else ()
            }
        };
        push!(turnLeft(direction), 1);
        push!(turnRight(direction), 1);
        push!(direction, steps + 1);
        MutList.toList(nextMoves)
    }

    @Test 
    pub def testPart1(): Bool = {
        assertEq(102, () -> runPart1("Day17Test"))
    }
}
