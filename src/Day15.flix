mod Day15 {
    use Utils.{assertEq, forEachInRange, getOrThrow, parseInt32OrThrow, readInputLines, listSplit, listSum, Throw}

    pub def part1(): Int64 \ {Throw, IO} = 
        readInputLines("Day15") |> List.toVector |> Vector.get(0) |> run(calculateHash)

    pub def part2(): Int64 \ {Throw, IO} = 
        readInputLines("Day15") |> List.toVector |> Vector.get(0) |> run(calculateHashMap)

    def run(part: List[String] -> Int64 \ e, line: String): Int64 \ e =
        line |> String.splitOn(substr=",") |> part

    def calculateHash(lines: List[String]): Int64 = 
        lines 
            |> List.map(hashOperation)
            |> listSum(0i64)

    def hashOperation(operation: String): Int64 =
        String.foldLeft((current, ch) -> Int64.remainder((current + Char.toBmpCodePoint(ch) |> Int32.toInt64) * 17i64, 256i64), 0i64, operation)

    pub def calculateHashMap(steps: List[String]): Int64 \ Throw = region rc {
        let boxes = Array.init(rc, _ -> MutList.empty(rc), 256);
        def execute(step: String) = {
            let operationTypeIndex = String.findIndexOfLeft(ch -> ch == '=' or ch == '-', step) |> getOrThrow;
            let label = String.slice(start=0, end=operationTypeIndex, step);
            let boxNum = hashOperation(label) |> Int64.tryToInt32 |> getOrThrow;
            let box = Array.get(boxNum, boxes);
            match String.charAt(operationTypeIndex, step) {
                case '=' => {
                    let focalLength = String.sliceRight(start=operationTypeIndex+1, step) |> parseInt32OrThrow;
                    match RandomAccess.findIndex(match (l, _) -> l == label, box) {
                        case None => MutList.push!((label, focalLength), box)
                        case Some(n) => put!((label, focalLength), n, box)
                    }
                }
                case '-' => MutList.retain!(match (l, _) -> l != label, box)
                case _ => bug!("Unhandled")
            }
        };
        List.forEach(execute, steps);
        def focusingPower(boxNum, box) = {
            def singleLensFocusingPower(slotNum, entry) = {
                let (_, focalLength) = entry;
                (1 + boxNum) * (1 + slotNum) * focalLength
            };
            MutList.mapWithIndex(rc, singleLensFocusingPower, box) |> MutList.sum
        };
        Array.mapWithIndex(rc, focusingPower, boxes) |> Array.sum |> Int32.toInt64
    }

    def put!(x: a, n: Int32, l: MutList[a, r]): Unit \ r = {
        let MutList.MutList(_, rxs, _) = l;
        let xs = deref rxs;
        Array.put(x, n, xs)
    }
            
    @Test 
    def testPart1(): Bool =
        Assert.eq(1320i64, run(calculateHash, "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"))

    @Test 
    def testPart2(): Bool =
        assertEq(145i64, () -> run(calculateHashMap, "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"))
}