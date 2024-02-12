def run(part: Int32): Unit \ IO =
    let inputs = match part {
        case 1 => digits()
        case 2 => numberWords()
        case _ => bug!("1 or 2")
    };
    Files.readLines("inputs/Day1.txt") |>
        Result.map(List.sumWith(readCalibrationValue(inputs))) |>
        println

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

def readCalibrationValue(prefixes: Vector[NamedNumber], line: String): Int32 =
    let firstIndex = indexOfFirstPrefix(line, prefixes);
    let lastIndex = indexOfFirstPrefixFromRight(line, prefixes);
    10 * firstIndex + lastIndex

def indexOfFirstPrefix(line: String, prefixes: Vector[NamedNumber]): Int32 =
    def numberOfPrefix(s: String): Option[Int32] = {
        let numberWithStartPoints = Vector.map(
            nv -> {
                {value = nv.number, index = String.indexOf({substr=nv.name}, s) |> Option.getWithDefault(100)}
            }, prefixes);
        let first = Vector.minimumBy((vi1, vi2) -> Order.compare(vi1.index, vi2.index), numberWithStartPoints);
        Option.map(x -> x.value, first)
    };
    List.findMap(numberOfPrefix, substrings(line)) |> getOrQuit

def indexOfFirstPrefixFromRight(line: String, prefixes: Vector[NamedNumber]): Int32 =
    let revLine = String.reverse(line);
    let revPrefixes = Vector.map(nv -> {name = String.reverse(nv.name) | nv}, prefixes);
    indexOfFirstPrefix(revLine, revPrefixes)

def substrings(s: String): List[String] =
    if (String.isEmpty(s)) List#{} else s :: (String.sliceRight({start=1}, s) |> substrings)

def getOrQuit(x: Option[a]): a =
    match x {
        case Some(a) => a
        case None => bug!("No value")
    }

@Test
def day1a(): Bool =
    Assert.eq(38, readCalibrationValue(digits(), "pqr3stu8vwx"))

@Test
def day1b_1(): Bool =
    Assert.eq(83, readCalibrationValue(numberWords(), "eightwothree"))

@Test
def day1b_2(): Bool =
    Assert.eq(14, readCalibrationValue(numberWords(), "zoneight234"))
