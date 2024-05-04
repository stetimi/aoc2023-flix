pub trait RandomAccess[m] {
    type Elm
    type Aef: Eff
    pub def nth(n: Int32, xs: m): Option[RandomAccess.Elm[m]] \ RandomAccess.Aef[m]
}

instance RandomAccess[Vector[a]] {
    type Elm = a
    type Aef = {}
    pub def nth(n: Int32, xs: Vector[a]): Option[a] = Vector.nth(n, xs)
}

instance RandomAccess[MutList[a, r]] {
    type Elm = a
    type Aef = { r }
    pub def nth(n: Int32, xs: MutList[a, r]): Option[a] \ r = MutList.nth(n, xs)
}

mod RandomAccess {
    pub def findIndex(f: a -> Bool, xs: m): Option[Int32] \ RandomAccess.Aef[m] 
        with RandomAccess[m] 
        where RandomAccess.Elm[m] ~ a = {
        def loop(n) = {
            match RandomAccess.nth(n, xs) {
                case None => None
                case Some(a) => if (f(a)) Some(n) else loop(n+1)
            }
        };
        loop(0)
    }
}