mod Utils {
    pub def unsafeHead(xs: List[a]): a =
        match xs {
            case Nil => bug!("empty")
            case x :: _ => x
        }

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

    pub def resultOrQuit(r: Result[e, a]): a =
        match r {
            case Ok(a) => a
            case Err(_) => bug!("none")
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
}