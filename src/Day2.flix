use Utils.{assertEq, getOrThrow, Throw};

def day2a(): Result[String, Int32] \ {IO, Throw} =
    Files.readLines("inputs/Day2.txt")
        |> Result.mapErr(_ -> "failed to read file")
        |> Result.map(sumPossibleGames)

def day2b(): Result[String, Int32] \ {IO, Throw} =
    Files.readLines("inputs/Day2.txt")
        |> Result.mapErr(_ -> "failed to read file")
        |> Result.map(sumPowers)

def sumPowers(lines: List[String]): Int32 \ Throw =
    List.sumWith(powerFromLine, lines)

def powerFromLine(line: String): Int32 \ Throw =
    let game = String.splitOn(substr={": "}, line);
    let drawsText = List.last(game) |> getOrThrow;
    let draws = parseMultipleDraws(drawsText);
    let drawsMinimalSize = mimimalSize(draws);
    power(drawsMinimalSize)

def power(draws: Map[String, Int32]): Int32 =
    Map.valuesOf(draws) |> List.foldRight((x, acc) -> x * acc, 1)

def mimimalSize(draws: List[Map[String, Int32]]): Map[String, Int32] =
    let colours = List#{"red", "green", "blue"};
    def foldFunc(m, acc): Map[String, Int32] = {
        def replace(key) = {
            let mValue = Map.getWithDefault(key, 0, m);
            let accValue = Map.getWithDefault(key, 0, acc);
            (key, Int32.max(mValue, accValue))
        };
        List.map(replace, colours) |> List.toMap
    };
    List.foldRight(foldFunc, Map#{}, draws)

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

@Test 
def testMinimalSize(): Bool =
    let draws = List#{
        Map#{"blue" => 3, "red" => 4},
        Map#{"red" => 1, "green" => 2},
        Map#{"blue" => 6, "green" => 2}
    };
    Assert.eq(Map#{"red" => 4, "green" => 2, "blue" => 6}, mimimalSize(draws))

@Test 
def testPower(): Bool =
    Assert.eq(48, power(Map#{"red" => 4, "green" => 2, "blue" => 6}))