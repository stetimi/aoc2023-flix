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

    pub def doIf(cond: Bool, action: Unit -> Unit \ e): Unit \ e =
        if (cond) {
            action()
        } else {
            ()
        }

    pub def setStringBuilderLength(newLength: Int32, sb: StringBuilder[r]): Unit =
        import java.lang.StringBuilder.setLength(Int32): Unit \ {} as setLength;
        let StringBuilder.StringBuilder(msb) = sb;
        setLength(msb, newLength)
}