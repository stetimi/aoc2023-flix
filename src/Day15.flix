mod Day15 {
    use Utils.{assertEq, forEachInRange, readInputLines, listSplit, listSum, Throw}

    pub def part1(): Int64 \ {Throw, IO} = 
        readInputLines("Day15") |> List.toVector |> Vector.get(0) |> calculateHash

    def calculateHash(input: String): Int64 = 
        input 
            |> String.splitOn(substr=",")
            |> List.map(String.foldLeft((current, ch) -> Int64.remainder((current + Char.toBmpCodePoint(ch) |> Int32.toInt64) * 17i64, 256i64), 0i64))
            |> listSum(0i64)

    @Test 
    def testPart1(): Bool =
        Assert.eq(1320i64, calculateHash("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"))
}