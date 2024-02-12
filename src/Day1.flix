eff Throw {
    pub def throw(): Void
}

def run(part: Int32): Unit \ IO =
    Files.readLines("inputs/Day1.txt")
        |> Result.mapErr(_ -> "failed to read file")
        |> Result.flatMap(runFromFileContents(part))
        |> println

def runFromFileContents(part: Int32, lines: List[String]): Result[String, Int32] =
    let words = match part {
        case 1 => Ok(digits())
        case 2 => Ok(numberWords())
        case _ => Err("1 or 2 please")
    };
    try {
        Result.map(w -> List.sumWith(readCalibrationValue(w), lines), words) 
    } with Throw {
        def throw(_k) = Err("Oops")
    }

type alias NamedNumber = {name = String, number = Int32}

def digits(): Vector[NamedNumber] = 
    Vector#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9} |> Vector.map(x -> {name = ToString.toString(x), number = x})

def numberWords(): Vector[NamedNumber] = 
    Vector#{
        {name = "zero", number = 0}, 
        {name = "one", number = 1}, 
        {name = "two", number = 2}, 
        {name = "three", number = 3}, 
        {name = "four", number = 4}, 
        {name = "five", number = 5}, 
        {name = "six", number = 6}, 
        {name = "seven", number = 7}, 
        {name = "eight", number = 8}, 
        {name = "nine", number = 9}} ++ digits()

def readCalibrationValue(prefixes: Vector[NamedNumber], line: String): Int32 \ Throw =
    let firstIndex = indexOfFirstPrefix(line, prefixes);
    let lastIndex = indexOfFirstPrefixFromRight(line, prefixes);
    10 * firstIndex + lastIndex

def indexOfFirstPrefix(line: String, prefixes: Vector[NamedNumber]): Int32 \ Throw =
    def numberOfPrefix(s: String): Option[Int32] = {
        let numberWithStartPoints = Vector.map(
            nv -> {
                {value = nv.number, index = String.indexOf({substr=nv.name}, s) |> Option.getWithDefault(100)}
            }, prefixes);
        let first = Vector.minimumBy((vi1, vi2) -> Order.compare(vi1.index, vi2.index), numberWithStartPoints);
        Option.map(x -> x.value, first)
    };
    List.findMap(numberOfPrefix, substrings(line)) |> getOrThrow

def indexOfFirstPrefixFromRight(line: String, prefixes: Vector[NamedNumber]): Int32 \ Throw =
    let revLine = String.reverse(line);
    let revPrefixes = Vector.map(nv -> {name = String.reverse(nv.name) | nv}, prefixes);
    indexOfFirstPrefix(revLine, revPrefixes)

def substrings(s: String): List[String] =
    if (String.isEmpty(s)) List#{} else s :: (String.sliceRight({start=1}, s) |> substrings)

def getOrThrow(x: Option[a]): a \ Throw =
    match x {
        case Some(a) => a
        case None => do Throw.throw()
    }

@Test
def day1a(): Bool =
    Assert.eq(Ok(38), runFromFileContents(1, List#{"pqr3stu8vwx"}))

@Test
def day1b_1(): Bool =
    Assert.eq(Ok(83), runFromFileContents(2, List#{"eightwothree"}))

@Test
def day1b_2(): Bool =
    Assert.eq(Ok(14), runFromFileContents(2, List#{"zoneight234"}))
