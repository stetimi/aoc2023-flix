mod Day9 {
    use Utils.{assertEq, getOrThrow, headOrThrow, parseInt32OrThrow, readInputLines, Throw}

    type alias Line = Vector[Int32]

    pub def part1(): Int32 \ {IO, Throw} = run("Day9", sumAllExtrapolated)
    
    pub def run(file: String, part: List[Line] -> Int32 \ Throw): Int32 \ {IO, Throw} =
        readInputLines(file)
            |> parseInput
            |> part  

    def parseInput(lines: List[String]): List[Line] \ Throw =
        List.map(String.splitOn({substr=" "}) 
                    >> List.map(parseInt32OrThrow) 
                    >> List.toVector, lines)

    def sumAllExtrapolated(lines: List[Line]): Int32 \ Throw =
        lines
            |> List.map(nextLines >> extrapolated >> headOrThrow)
            |> List.sum

    def extrapolated(lines: List[Line]): List[Int32] \ Throw =
        def loop(thisLines: List[Line], last, all): List[Int32] \ Throw = {
            match thisLines {
                case Nil => all
                case Cons(line, rest) =>
                    let nextLast = last + Vector.last(line) |> getOrThrow;
                    let next = Cons(nextLast, all);  
                    loop(rest, nextLast, next)
            }
        };
        loop(lines, 0, List#{})

    def nextLines(line: Line): List[Line] =
        def loop(thisLine, all: List[Line]) = {
            let diffs = Vector.zip(thisLine, Vector.dropLeft(1, thisLine))
                |> Vector.map(match (x1, x2) -> x2 - x1);
            let res = Cons(diffs, all);
            if (Vector.forAll(x -> x == 0, diffs)) {
                res
            } else {
                loop(diffs, res)
            }
        };
        loop(line, List#{line})


    @Test 
    pub def testPart1(): Bool \ IO =
        assertEq(114, () -> run("Day9Test", sumAllExtrapolated))

    @Test 
    pub def testNextLines(): Bool =
        let line = Vector#{1, 3, 6, 10, 15, 21};
        let expected = List#{
            Vector#{0, 0, 0},
            Vector#{1, 1, 1, 1},
            Vector#{2, 3, 4, 5, 6},
            line
        };
        Assert.eq(expected, nextLines(line))

    @Test 
    pub def testExtrapolated(): Bool =
        let diffed = List#{
            Vector#{0, 0, 0},
            Vector#{1, 1, 1, 1},
            Vector#{2, 3, 4, 5, 6},
            Vector#{1, 3, 6, 10, 15, 21}
        };
        assertEq(List#{28, 7, 1, 0}, () -> extrapolated(diffed))
}