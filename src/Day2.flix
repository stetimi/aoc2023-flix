def day2a(): Result[String, Int32] \ {IO, Throw} =
    Files.readLines("inputs/Day2.txt")
        |> Result.mapErr(_ -> "failed to read file")
        |> Result.map(sumPossibleGames)

def sumPossibleGames(lines: List[String]): Int32 \ Throw =
    List.map(possibleGameId, lines) 
        |> List.sumWith(Option.getWithDefault(0))

def isPossible(draw: Map[String, Int32]): Bool =
    def get(colour) = Map.getWithDefault(colour, 0, draw);
    get("red") <= 12 and get("green") <= 13 and get("blue") <= 14

def possibleGameId(text: String): Option[Int32] \ Throw =
    let game = String.splitOn(substr={": "}, text);
    let idPart = List.head(game) |> getOrThrow |> String.splitOn(substr={" "}) |> List.reverse |> stringInt32;
    let noPossibleDraws = List.last(game) |> getOrThrow |> parseMultipleDraws |> List.exists(g -> not isPossible(g));
    if (noPossibleDraws) None else Some(snd(idPart)) 

def parseMultipleDraws(text: String): List[Map[String, Int32]] \ Throw =
    String.splitOn({substr="; "}, text)
        |> List.map(parseSingleDraw)

def parseSingleDraw(text: String): Map[String, Int32] \ Throw =
    String.splitOn({substr=", "}, text) 
        |> List.map(String.splitOn({substr=" "}))   
        |> List.map(stringInt32)
        |> List.toMap

def stringInt32(l: List[String]): (String, Int32) \ Throw =
    let s = List.last(l) |> getOrThrow;
    let n = List.head(l) |> getOrThrow |> Int32.parse(10) |> Result.toOption |> getOrThrow;
    (s, n)

def assertEq(expected: a, f: Unit -> a \ Throw): Bool with ToString[a], Eq[a] =
    try {
        Assert.eq(expected, f())
    } with Throw {
        def throw(_k) = false
    }

@Test 
def testParseSingleDraw(): Bool =
    assertEq(Map#{"green" => 8, "blue" => 6, "red" => 20}, () -> parseSingleDraw("8 green, 6 blue, 20 red"))

@Test
def testPossibleGameId(): Bool =
    assertEq(Some(1), () -> possibleGameId("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")) and
    assertEq(None, () -> possibleGameId("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"))

@Test
def day2aSample(): Bool =
    let sample = List#{
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", 
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    };
    assertEq(8, () -> sumPossibleGames(sample))