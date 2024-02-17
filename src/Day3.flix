use Utils.{assertEq, doIf, getOrThrow, resultOrThrow, setStringBuilderLength, Throw};

type alias Grid = {width = Int32, chars = Vector[Char]}
type alias NumberPosition = {index = Int32, text = String}

def day3a(): Int32 \ {Throw, IO} =
    Files.readLines("inputs/Day3.txt") 
        |> resultOrThrow
        |> toGrid
        |> sumPartNumbers

def toGrid(lines: List[String]): Grid =
    let width = Utils.unsafeHead(lines) |> String.length;
    let linesVec = List.toVector(lines) |> Vector.map(String.toVector);
    {width = width, chars = Vector.foldRight((v, acc) -> v ++ acc, Vector#{}, linesVec)}

def sumPartNumbers(grid: Grid): Int32 \ Throw =
    let partNumberIndices = coordsAdjacentToSymbol(grid);
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
                let stringAtEndOfRow = Int32.rem(index + 1, grid.width) == 0 and StringBuilder.length(numberText) != 0;
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

def coordsAdjacentToSymbol(grid: Grid): Set[Int32] =
    region r {
        let coords = MutSet.new(r);
        def noteSymbol(i, ch) = 
            doIf(ch != '.' and not Char.isDigit(ch), () -> {
                let adjacents = adjacentCoords(i, grid.width);
                MutSet.addAll!(adjacents, coords)
            });
        Vector.forEachWithIndex(noteSymbol, grid.chars);
        MutSet.toSet(coords)
    }

def adjacentCoords(n: Int32, width: Int32): Set[Int32] =
    if (Int32.rem(n, width) == 0) {
        Set#{n - width, n - width + 1,
                        n + 1,
             n + width, n + width + 1}
    } else if (Int32.rem(n, width) == width - 1) {
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
    let grid = toGrid(lines);
    Assert.eq(3, grid.width) and Assert.eq(String.toVector("abcdef"), grid.chars)

@Test 
def testCoordsAdjacentToSymbol(): Bool =
    // 0123
    // 45@7
    // 8901
    let grid = {width = 4, chars = String.toVector("......@.....")};
    let coords = coordsAdjacentToSymbol(grid);
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

@Test 
def day3aTestSet(): Bool = 
    let lines = List#{
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
    };
    let grid = toGrid(lines);
    assertEq(4361, () -> sumPartNumbers(grid))