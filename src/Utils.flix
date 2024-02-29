mod Utils {
    pub def unsafeHead(xs: List[a]): a =
        match xs {
            case Nil => bug!("empty")
            case x :: _ => x
        }

    pub def unsafeParseInt32(text: String): Int32 =
        Int32.parse(10, text) |> resultOrQuit("Cannot parse '${text}' to Int32")

    pub def unsafeParseInt64(text: String): Int64 =
        Int64.parse(10, text) |> resultOrQuit("Cannot parse '${text}' to Int64")

    pub eff Throw {
        pub def throw(): Void
    }

    pub def getOrThrow(x: Option[a]): a \ Throw =
        match x {
            case Some(a) => a
            case None => do Throw.throw()
        }

    pub def getOrQuit(x: Option[a]): a =
        match x {
            case Some(a) => a
            case None => bug!("can't be none")
        }

    pub def assertEq(expected: a, f: Unit -> a \ Throw): Bool with ToString[a], Eq[a] =
        try {
            Assert.eq(expected, f())
        } with Throw {
            def throw(_k) = false
        }  

    pub def resultOrThrow(r: Result[e, a]): a \ Throw =
        match r {
            case Ok(a) => a
            case Err(_) => do Throw.throw()
        }

    pub def resultOrQuit(msg: String, r: Result[e, a]): a =
        match r {
            case Ok(a) => a
            case Err(_) => bug!(msg)
        }

    pub def doIf(cond: Bool, action: Unit -> Unit \ e): Unit \ e =
        if (cond) {
            action()
        } else {
            ()
        }

    pub def listProduct(xs: List[Int32]): Int32 =
        List.foldRight((x, y) -> x * y, 1, xs)

    pub def setStringBuilderLength(newLength: Int32, sb: StringBuilder[r]): Unit =
        import java.lang.StringBuilder.setLength(Int32): Unit \ {} as setLength;
        let StringBuilder.StringBuilder(msb) = sb;
        setLength(msb, newLength)

    pub def readInputLines(day: Int32): List[String] \ {IO, Throw} =
        Files.readLines("inputs/Day${day}.txt") 
            |> resultOrThrow

    pub def uncheckedReadInputLines(filename: String): List[String] =
        unchecked_cast(Files.readLines("inputs/${filename}.txt") |> resultOrQuit("Cannot read filename ${filename}") as _ \ {}) 

        pub def listSplit(matches: a -> Bool, xs: List[a]): List[List[a]] =
        region r {
            let curr = ref MutList.new(r) @ r;
            let split = MutList.new(r) !> MutList.push!(deref curr);
            foreach(x <- xs) {
                if(matches(x)) {
                    curr := MutList.new(r);
                    MutList.push!(deref curr, split)
                } else {
                    MutList.push!(x, deref curr)
                }
            };
            MutList.toList(split) |> List.map(MutList.toList)
        }

    pub def loop(start: Int64, end: Int64, f: Int64 -> Bool \ e): Unit \ e =
        if (start < end) {
            if (f(start)) {
                loop(start + 1i64, end, f)
            } else {
                ()
            }
        } else {
            ()
        }

    pub def prod(unit: a, xs: List[a]): a with Mul[a] =
        List.foldRight((x, y) -> x * y, unit, xs)

    @Test   
    pub def testListSplit(): Bool = 
        let xs = List#{1, 2, 3, 4, 5};
        let split = listSplit(x -> x == 3 or x == 4, xs);
        Assert.eq(List#{List#{1, 2}, List.Nil, List#{5}}, split)  

    @Test   
    pub def testProd(): Bool = 
        Assert.eq(70, prod(1, List#{2, 5, 7}))
}