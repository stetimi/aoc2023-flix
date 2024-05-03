enum PriorityQueue[a: Type, r: Eff](Ref[Array[a, r], r], Ref[Int32, r], (a, a) -> Comparison)

mod PriorityQueue {
    pub def empty(rc: Region[r]): PriorityQueue[a, r] \ r with Order[a] = {
        PriorityQueue(ref Array.empty(rc, 8) @ rc, ref 0 @ rc, Order.compare)
    }

    pub def emptyWithComparison(rc: Region[r], cmp: (a, a) -> Comparison): PriorityQueue[a, r] \ r = {
        PriorityQueue(ref Array.empty(rc, 8) @ rc, ref 0 @ rc, cmp)
    }

    pub def size(pq: PriorityQueue[a, r]): Int32 \ r = {
        let PriorityQueue.PriorityQueue(_, rs, _) = pq;
        deref rs
    }

    pub def toString(pq: PriorityQueue[a, r]): String \ r with ToString[a] = region rc {
        let PriorityQueue.PriorityQueue(ra, rs, _) = pq;
        let contents = deref ra 
            |> Array.slice(rc, start=0, end=deref rs)
            |> Array.join(",");
        "PriorityQueue(${contents})"
    }

    pub def toSortedList(pq: PriorityQueue[a, r]): List[a] \ r = {
        def loop(xs) = 
            match pop!(pq) {
                case None => xs
                case Some(x) => loop(x :: xs)
            };
        loop(Nil)
    }

    /// rc is needed as the underlying array may need to be reallocated.
    pub def insert!(rc: Region[r], x: a, pq: PriorityQueue[a, r]): Unit \ r = { 
        let PriorityQueue.PriorityQueue(ra, rs, _) = pq;
        let s = deref rs;
        if (s >= Array.length(deref ra)) {
            let newArray = Array.empty(rc, s * 2);
            Array.forEachWithIndex((i, x1) -> Array.put(x1, i, newArray), deref ra);
            ra := newArray
        } else ();
        siftUp!(size(pq), x, pq)
    }

    /// Never shrinks the underlying array.
    pub def pop!(pq: PriorityQueue[a, r]): Option[a] \ r = {
        let PriorityQueue.PriorityQueue(ra, rs, _) = pq;
        let a = deref ra;
        if (deref rs == 0) {
            None
        } else {
            let result = Array.get(0, a);
            rs := deref rs - 1;
            let x = Array.get(deref rs, a);
            if (deref rs > 0) {
                siftDown!(0, x, pq)
            } else ();
            Some(result)
        }
    }

    pub def top(pq: PriorityQueue[a, r]): Option[a] \ r =
        let PriorityQueue.PriorityQueue(ra, rs, _) = pq;
        if (deref rs == 0) {
            None
        } else {
            Some(Array.get(deref rs, deref ra))
        }


    def siftUp!(k: Int32, x: a, pq: PriorityQueue[a, r]): Unit \ r = {
        let PriorityQueue.PriorityQueue(ra, rs, cmp) = pq;
        let a = deref ra;
        def loop(k1) = {
            if (k1 > 0) {
                let parent = (k1 - 1) / 2;
                let e = Array.get(parent, a);
                if (cmp(x, e) == Comparison.GreaterThan) {
                    k1  
                } else {
                    Array.put(e, k1, a);
                    loop(parent)
                }
            } else k1
        };
        let insertion = loop(k);
        Array.put(x, insertion, a);
        rs := deref rs + 1
    }

    def siftDown!(k: Int32, x: a, pq: PriorityQueue[a, r]): Unit \ r = region rc {
        let PriorityQueue.PriorityQueue(ra, rs, cmp) = pq;
        let n = deref rs;
        let a = deref ra;
        let half = n / 2;
        def loop(k1) = {
            if (k1 < half) {
                let rchild = ref (k1 * 2 + 1) @ rc;
                let rchildElem = ref Array.get(deref rchild, a) @ rc;
                let right = 1 + deref rchild;
                if (right < n and cmp(deref rchildElem, Array.get(right, a)) == Comparison.GreaterThan) {
                    rchildElem := Array.get(right, a);
                    rchild := right
                } else ();
                if (cmp(x, deref rchildElem) != Comparison.GreaterThan) {
                    k1
                } else {
                    Array.put(deref rchildElem, k1, a);
                    loop(deref rchild)
                }
            } else k1
        };
        let insertion = loop(k); 
        Array.put(x, insertion, a)
    }
}

@Test 
def emptyQueueHasSize0(): Bool = region rc {
    let pq: PriorityQueue[Int32, _] = PriorityQueue.empty(rc);
    Assert.eq(0, pq |> PriorityQueue.size)
}

@Test 
def toStringOnEmpty(): Bool = region rc {
    let pq: PriorityQueue[Int32, _] = PriorityQueue.empty(rc);
    Assert.eq("PriorityQueue()", PriorityQueue.toString(pq))
}

@Test 
def insert1(): Bool = region rc {
    let pq = PriorityQueue.empty(rc);
    PriorityQueue.insert!(rc, 10, pq);
    Assert.eq("PriorityQueue(10)", PriorityQueue.toString(pq))
}

@Test 
def insertMany(): Bool = region rc {
    let pq = PriorityQueue.empty(rc);
    repeat(10, n -> PriorityQueue.insert!(rc, n, pq));
    Assert.eq("PriorityQueue(1,2,5,4,3,9,6,10,7,8)", PriorityQueue.toString(pq))
}

@Test
def popEmptyGivesNone(): Bool = region rc {
    let pq: PriorityQueue[Int32, _] = PriorityQueue.empty(rc);
    let res = pq |> PriorityQueue.pop!;
    Assert.eq(None, res)
}

@Test
def popOnSize1(): Bool = region rc {
    let pq = PriorityQueue.empty(rc);
    PriorityQueue.insert!(rc, 5, pq);
    Assert.eq(Some(5), PriorityQueue.pop!(pq))
}

@Test
def popOnSize2(): Bool = region rc {
    let pq = PriorityQueue.empty(rc);
    PriorityQueue.insert!(rc, 15, pq);
    PriorityQueue.insert!(rc, 5, pq);
    Assert.eq(Some(5), PriorityQueue.pop!(pq)) and
    Assert.eq(Some(15), PriorityQueue.pop!(pq))
}

@Test
def smallListTest(): Bool = region rc {
    let pq = PriorityQueue.empty(rc);
    def insert!(x) = PriorityQueue.insert!(rc, x, pq);
    insert!(2);
    insert!(3);
    insert!(-1);
    insert!(3);
    insert!(11);
    insert!(8);
    Assert.eq(List#{11, 8, 3, 3, 2, -1}, PriorityQueue.toSortedList(pq))
}

@Test
def bigListTest(): Bool \ IO = region rc {
    let pq = PriorityQueue.empty(rc);
    let rnd = Random.freshWithSeed(10i64);
    repeat(1000, _ -> Random.nextInt32(rnd) |> n -> PriorityQueue.insert!(rc, n, pq));
    let actual = PriorityQueue.toSortedList(pq);
    let sorted = List.sort(actual) |> List.reverse;
    Assert.eq(sorted, actual)
}

@Test
def withComparison(): Bool = region rc {
    let pq = PriorityQueue.emptyWithComparison(rc, (x, y) -> Order.compare(String.length(x), String.length(y)));
    PriorityQueue.insert!(rc, "12345", pq);
    PriorityQueue.insert!(rc, "543", pq);
    PriorityQueue.insert!(rc, "", pq);
    PriorityQueue.insert!(rc, "9", pq);
    Assert.eq(List#{"12345", "543", "9", ""}, PriorityQueue.toSortedList(pq))
}

def repeat(n: Int32, f: Int32 -> a \ r): Unit \ r =
    if (n <= 0) () else {
        f(n);
        repeat(n - 1, f)
    }