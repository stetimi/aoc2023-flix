mod Day14 {
    use Utils.{assertEq, forEachInRange, readInputLines, listSplit, Throw}

    pub def part1(): Int32 \ {Throw, IO} = 
        run(readInputLines("Day14"), tilt >> transpose >> calculateLoad)

    pub def part2(): Int32 \ {Throw, IO} = 
        run(readInputLines("Day14"), cycle)

    pub def run(lines: List[String], part: Vector[Vector[Char]] -> Int32): Int32 =
        lines
            |> List.toVector
            |> Vector.map(String.toVector)
            |> part

    pub def cycle(rows :Vector[Vector[Char]]): Int32 = region rc {
        let spin = tilt >> rotate90Clockwise >> tilt >> rotate90Clockwise >> tilt >> rotate90Clockwise >> tilt >> rotate90Clockwise;
        let seen = MutMap.empty(rc);
        def loop(count, rs) = 
            match MutMap.get(rs, seen) {
                case None => {
                    MutMap.put!(rs, count, seen);
                    loop(count + 1, rs |> spin)
                }
                case Some(startCount) => (startCount, count, rs)
            };
        let (startCount, endCount, fixPoint) = loop(0, rows);
        let cycleLen = endCount - startCount;
        let numCycles = (1_000_000_000 - startCount) / cycleLen;    
        let remainingSpins = (1_000_000_000 - startCount) - (cycleLen * numCycles);
        def finalSpin(count, rs) =
            if (count == 0) {
                rs
            } else {
                finalSpin(count - 1, spin(rs))
            };
        let finalGrid = finalSpin(remainingSpins, fixPoint);
        calculateLoad(finalGrid |> transpose)
    }

    def _printGrid(rows: Vector[Vector[Char]]): Int32 =
        let v = unchecked_cast(rows |> Vector.map(Vector.join("")) |> Vector.map(println) as _ \ {});
        Vector.length(v)

    pub def tilt(rows: Vector[Vector[Char]]): Vector[Vector[Char]] = 
        rows
            |> transpose
            |> Vector.map(tiltCol)
            |> transpose

    pub def tiltCol(col: Vector[Char]): Vector[Char] =
        let height = Vector.length(col);
        def move(i, buffer, out: Vector[Char]) = {
            if (i == height) {
                out ++ buffer
            } else {
                let ch = Vector.get(i, col);
                match ch {
                    case '.' => move(i + 1, buffer ++ Vector#{ch}, out)
                    case '#' => move(i + 1, Vector#{}, out ++ buffer ++ Vector#{ch})
                    case 'O' => move(i + 1, buffer, out ++ Vector#{ch})
                    case _ => ???
                }
            }
        };
        move(0, Vector#{}, Vector#{})

    pub def calculateLoad(cols: Vector[Vector[Char]]): Int32 = 
        let height = Vector.get(0, cols) |> Vector.length;
        def calculateRowLoad(i) = {
            let rocksCount = cols 
                |> Vector.map(Vector.get(i))
                |> Vector.count(ch -> ch == 'O');
            rocksCount * (height - i)
        };
        Vector.range(0, height)
            |> Vector.sumWith(calculateRowLoad)

    def transpose(rows: Vector[Vector[a]]): Vector[Vector[a]] = region rc {
        let height = Vector.length(rows);
        let width = rows |> Vector.get(0) |> Vector.length;
        let transposed = Array.init(rc, _ -> Array.empty(rc, height), width);
        def put!(row, col, value): Unit = 
            Array.get(col, transposed) |> Array.put(value, row);
        rows |> Vector.forEachWithIndex((iRow, row) -> {
            row |> Vector.forEachWithIndex((iCol, value) -> {
                put!(iRow, iCol, value)
            })
        });
        Array.toVector(transposed) |> Vector.map(Array.toVector)
    }

    def rotate90Clockwise(rows: Vector[Vector[a]]): Vector[Vector[a]] = 
        rows 
            |> transpose
            |> Vector.map(Vector.reverse)

    @Test 
    pub def testPart1(): Bool = 
        assertEq(136, () -> run(readInputLines("Day14Test"), tilt >> transpose >> calculateLoad))

    @Test 
    pub def testPart2(): Bool = 
        assertEq(64, () -> run(readInputLines("Day14Test"), cycle))

    @Test 
    pub def testTranspose(): Bool =
        let vs = Vector#{
            Vector#{1,2,3},
            Vector#{4,5,6}
        };
        let expected = Vector#{
            Vector#{1,4}, Vector#{2,5}, Vector#{3,6}
        };
        Assert.eq(expected, transpose(vs))

    @Test 
    pub def testRotate90Clockwise(): Bool =
        let vs = Vector#{
            Vector#{1,2,3},
            Vector#{4,5,6},
            Vector#{7,8,9}
        };
        let expected = Vector#{
            Vector#{7,4,1}, 
            Vector#{8,5,2}, 
            Vector#{9,6,3}
        };
        Assert.eq(expected, rotate90Clockwise(vs))
}