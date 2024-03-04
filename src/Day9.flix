mod Day9 {
    use Utils.{getOrQuit, unsafeHead, unsafeParseInt32, uncheckedReadInputLines}

    type alias Line = Vector[Int32]

    pub def part1(file: String): Int32 =
        uncheckedReadInputLines(file)
            |> parseInput
            |> sumAllExtrapolated

    def parseInput(lines: List[String]): List[Line] =
        lines
            |> List.map(String.splitOn({substr=" "}))
            |> List.map(List.map(unsafeParseInt32))
            |> List.map(List.toVector)

    def sumAllExtrapolated(lines: List[Line]): Int32 =
        lines
            |> List.map(nextLines)
            |> List.map(extrapolated)
            |> List.map(unsafeHead)
            |> List.sum

    def extrapolated(lines: List[Line]): List[Int32] =
        def loop(thisLines: List[Line], last, all) = {
            match thisLines {
                case Nil => all
                case Cons(line, rest) =>
                    let nextLast = last + Vector.last(line) |> getOrQuit;
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
    pub def testPart1(): Bool =
        Assert.eq(114, part1("Day9Test"))

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
        Assert.eq(List#{28, 7, 1, 0}, extrapolated(diffed))

}