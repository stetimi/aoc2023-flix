mod Utils {
    pub def headOrThrow(xs: List[a]): a \ Throw =
        match xs {
            case Nil => do Throw.throw()
            case x :: _ => x
        }   

    pub def parseInt32OrThrow(text: String): Int32 \ Throw =
        Int32.parse(10, text) |> resultOrThrow

    pub def parseInt64OrThrow(text: String): Int64 \ Throw =
        Int64.parse(10, text) |> resultOrThrow

    pub eff Throw {
        pub def throw(): Void
    }

    pub def getOrThrow(x: Option[a]): a \ Throw =
        match x {
            case Some(a) => a
            case None => do Throw.throw()
        }

    pub def assertEq(expected: a, f: Unit -> a \ {e, Throw}): Bool with ToString[a], Eq[a] =
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

    pub def doIf(cond: Bool, action: Unit -> Unit \ e): Unit \ e =
        if (cond) {
            action()
        } else {
            ()
        }

    pub def negateComparison(c: Comparison): Comparison =
        match c {
            case Comparison.EqualTo => Comparison.EqualTo
            case Comparison.LessThan => Comparison.GreaterThan
            case Comparison.GreaterThan => Comparison.LessThan
        }

    pub def readInputLines(filename: String): List[String] \ {IO, Throw} =
        Files.readLines("inputs/${filename}.txt") 
            |> resultOrThrow

    pub def listSplit(matches: a -> Bool, xs: List[a]): List[List[a]] =
        region r {
            let curr = ref MutList.empty(r) @ r;
            let split = MutList.empty(r) !> MutList.push!(deref curr);
            foreach(x <- xs) {
                if(matches(x)) {
                    curr := MutList.empty(r);
                    MutList.push!(deref curr, split)
                } else {
                    MutList.push!(x, deref curr)
                }
            };
            MutList.toList(split) |> List.map(MutList.toList)
        }

    pub def loop(start: a, end: a, f: a -> Bool \ e): Unit \ e with Order[a], Inc[a] =
        if (start < end) {
            if (f(start)) {
                loop(Inc.inc(start), end, f)
            } else {
                ()
            }
        } else {
            ()
        }

    pub trait Inc[a] {
        pub def inc(a: a): a
    }

    instance Inc[Int64] {
        pub def inc(x: Int64): Int64 = x + 1i64
    }

    instance Inc[Int32] {
        pub def inc(x: Int32): Int32 = x + 1
    }

    pub def prod(unit: a, xs: List[a]): a with Mul[a] =
        List.foldRight((x, y) -> x * y, unit, xs)

    pub def listSum(unit: a, xs: List[a]): a with Add[a] =
        List.foldRight((x, y) -> x + y, unit, xs)

    pub def vectorSum(unit: a, xs: Vector[a]): a with Add[a] =
        Vector.foldRight((x, y) -> x + y, unit, xs)

    pub def comparisonOnOrdered(x: a, y: a): Comparison with Order[a] =
        if (x < y) {
            Comparison.LessThan
        } else if (x > y) {
            Comparison.GreaterThan
        } else {
            Comparison.EqualTo
        }

    pub def comparing(mkKey: a -> k, first: a, second: a): Comparison with Order[k] =
        comparisonOnOrdered(mkKey(first), mkKey(second))

    pub def stringForEach(f: Char -> Unit \ e, s: String): Unit \ e =
        let l = String.length(s);
        def loop(i) =
            if (i >= l) {
                ()
            } else {
                f(String.charAt(i, s));
                loop(i + 1)
            };
        loop(0)

    pub def forEachInRange(start: Int32, end: Int32, f: Int32 -> Unit \ e): Unit \ e = 
        def loop(i) =
            if (i >= end) {
                ()
            } else {
                f(i);
                loop(i + 1)
            };
        loop(start)

    @Test   
    pub def testListSplit(): Bool = 
        let xs = List#{1, 2, 3, 4, 5};
        let split = listSplit(x -> x == 3 or x == 4, xs);
        Assert.eq(List#{List#{1, 2}, List.Nil, List#{5}}, split)  

    @Test   
    pub def testProd(): Bool = 
        Assert.eq(70, prod(1, List#{2, 5, 7}))
}