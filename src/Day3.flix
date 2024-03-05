mod Day3 {
    use Utils.{assertEq, doIf, getOrThrow, headOrThrow, prod, readInputLines, resultOrThrow, setStringBuilderLength, Throw};

    type alias Grid = {width = Int32, chars = Vector[Char]}
    type alias NumberPosition = {index = Int32, text = String}

    pub def day3a(): Int32 \ {Throw, IO} =
        readInputLines("Day3") |> toGrid |> sumPartNumbers

    pub def day3b(): Int32 \ {Throw, IO} =
        readInputLines("Day3") |> toGrid |> sumGearRatios

    def toGrid(lines: List[String]): Grid \ Throw =
        let width = headOrThrow(lines) |> String.length;
        let linesVec = List.toVector(lines) |> Vector.map(String.toVector);
        {width = width, chars = Vector.foldRight((v, acc) -> v ++ acc, Vector#{}, linesVec)}

    def sumGearRatios(grid: Grid): Int32 \ Throw =
        let possibleNumberIndices = coordsAdjacentToSymbol(ch -> ch == '*', grid);
        let positions = numberPositions(grid);
        positionsSurroundingGear(positions, possibleNumberIndices)
            |> List.filter(gis -> List.length(gis) == 2)
            |> List.map(List.map(pn -> Int32.fromString(pn.text) |> getOrThrow))
            |> List.map(prod(1))
            |> List.sum

    def positionsSurroundingGear(numberPositions: List[NumberPosition], gearIndices: Map[Int32, Set[Int32]]): List[List[NumberPosition]] \ Throw =
        region r {
            let indicesToPositions = MutMap.new(r);
            def put!(index: Int32, np: NumberPosition) = {
                let nps = MutMap.get(index, indicesToPositions) |> Option.getWithDefault(List#{});
                MutMap.put!(index, np :: nps, indicesToPositions)
            };
            foreach(gearIndex <- Map.keysOf(gearIndices)) {
                let theseGearIndices = Map.get(gearIndex, gearIndices) |> getOrThrow;
                foreach(numberPosition <- numberPositions) {
                    doIf(isPartNumber(theseGearIndices, numberPosition), () -> 
                        put!(gearIndex, numberPosition)
                    )
                }
            };
            MutMap.toMap(indicesToPositions) |> Map.valuesOf
        }

    def sumPartNumbers(grid: Grid): Int32 \ Throw =
        let partNumberIndices = coordsAdjacentToSymbol(ch -> ch != '.', grid) |> Map.valuesOf |> List.toSet |> Set.flatten;
        let positions = numberPositions(grid);
        let partNumberPositions = List.filter(isPartNumber(partNumberIndices), positions);
        let partNumbers = List.map(pn -> Int32.fromString(pn.text) |> getOrThrow, partNumberPositions);
        List.sum(partNumbers)

    def isPartNumber(partNumberPositions: Set[Int32], np: NumberPosition): Bool =
        def go(i) = {
            if (i < String.length(np.text)) {
                if (Set.memberOf(np.index + i, partNumberPositions)) true else go(i + 1)
            } else false
        };
        go(0)

    def numberPositions(grid: Grid): List[NumberPosition] =
        region r {
            let numberPositions = MutList.new(r);
            let indexRef = ref 0 @ r;
            let numberText = StringBuilder.new(r);
            def run() = {
                let index = deref indexRef;
                doIf(index < Vector.length(grid.chars), () -> 
                    let ch = Vector.get(index, grid.chars);
                    let isDigit = Char.isDigit(ch);
                    doIf(isDigit, () -> StringBuilder.append!(ch, numberText));
                    let stringAtEndOfRow = Int32.remainder(index + 1, grid.width) == 0 and StringBuilder.length(numberText) != 0;
                    doIf(not isDigit or stringAtEndOfRow, () -> 
                        let numberTextLength = StringBuilder.length(numberText);
                        doIf(numberTextLength != 0, () -> 
                            let np = {index = index - numberTextLength, text = StringBuilder.toString(numberText)};
                            MutList.push!(np, numberPositions);
                            setStringBuilderLength(0, numberText)
                        )
                    );
                    indexRef := index + 1;
                    run()
                )
            };
            run();
            MutList.toList(numberPositions)
        }

    def coordsAdjacentToSymbol(isSymbol: Char -> Bool, grid: Grid): Map[Int32, Set[Int32]] =
        region r {
            let coords = MutMap.new(r);
            def addAll!(k, v: Set[Int32]): Unit = {
                let vs = MutMap.getWithDefault(k, Set#{}, coords);
                let nextVs = Set.union(v, vs);
                MutMap.put!(k, nextVs, coords)
            };
            def noteSymbol(i, ch) = 
                doIf(isSymbol(ch) and not Char.isDigit(ch), () -> {
                    let adjacents = adjacentCoords(i, grid.width);
                    addAll!(i, adjacents)
                });
            Vector.forEachWithIndex(noteSymbol, grid.chars);
            coords |> MutMap.toMap
        }

    def adjacentCoords(n: Int32, width: Int32): Set[Int32] =
        if (Int32.remainder(n, width) == 0) {
            Set#{n - width, n - width + 1,
                            n + 1,
                n + width, n + width + 1}
        } else if (Int32.remainder(n, width) == width - 1) {
            Set#{n - width - 1, n - width,
                n - 1       ,             
                n + width - 1, n + width}
        } else {
            Set#{n - width - 1, n - width, n - width + 1,
                n - 1       ,             n + 1,
                n + width - 1, n + width, n + width + 1}
        }

    @Test 
    def testToGrid(): Bool = 
        let lines = List#{"abc", "def"};
        assertEq(3, () -> toGrid(lines).width)
        and assertEq(String.toVector("abcdef"), () -> toGrid(lines).chars)

    @Test 
    def testCoordsAdjacentToSymbol(): Bool =
        // 0123
        // 45@7
        // 8901
        let grid = {width = 4, chars = String.toVector("......@.....")};
        let coords = coordsAdjacentToSymbol(ch -> ch != '.', grid) |> Map.valuesOf |> List.toSet |> Set.flatten;
        Assert.eq(Set#{1, 2, 3, 5, 7, 9, 10, 11}, coords)

    @Test 
    def testNumberPositions(): Bool =
        // .....
        // .123.
        // 4....
        //                                              012345678901234
        let grid = {width = 5, chars = String.toVector("......123.4....")};
        let nps = numberPositions(grid) |> List.toVector;
        Assert.eq(2, Vector.length(nps)) and
        Assert.eq(6, Vector.get(0, nps).index) and
        Assert.eq("123", Vector.get(0, nps).text) and
        Assert.eq(10, Vector.get(1, nps).index) and
        Assert.eq("4", Vector.get(1, nps).text)

    @Test 
    def testIsPartNumberWhenTrue(): Bool =
        let np = {index = 5, text = "123"};
        Assert.eq(true, isPartNumber(Set#{7, 8}, np))

    @Test 
    def testIsPartNumberWhenFalse(): Bool =
        let np = {index = 5, text = "123"};
        Assert.eq(false, isPartNumber(Set#{8}, np))  

    def day3Sample(): List[String] =
        List#{
            "467..114..",
            "...*......",
            "..35..633.",
            "......#...",
            "617*......",
            ".....+.58.",
            "..592.....",
            "......755.",
            "...$.*....",
            ".664.598.."
        }

    @Test 
    def day3aTestSet(): Bool = 
        assertEq(4361, () -> day3Sample() |> toGrid |> sumPartNumbers)

    @Test 
    def day3bTestSet(): Bool =
        assertEq(467835, () -> day3Sample() |> toGrid |> sumGearRatios)
}
    