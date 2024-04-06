mod Day14 {
    use Utils.{assertEq, forEachInRange, readInputLines, listSplit, Throw}

    pub def part1(): Int32 \ {Throw, IO} = 
        run(readInputLines("Day14"), tilt)

    pub def run(lines: List[String], part: Vector[Vector[Char]] -> Int32): Int32 =
        lines
            |> Grid.toGrid
            |> Grid.transpose
            |> Grid.rows
            |> part

    pub def tilt(cols: Vector[Vector[Char]]): Int32 = 
        cols 
            |> Vector.map(tiltCol)
            |> calculateLoad

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

    @Test 
    def testPart1(): Bool \ IO = 
        assertEq(136, () -> run(readInputLines("Day14Test"), tilt))
}