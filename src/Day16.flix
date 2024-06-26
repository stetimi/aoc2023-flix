mod Day16 {
    use Utils.{assertEq, forEachInRange, getOrThrow, parseInt32OrThrow, readInputLines, listSplit, listSum, Throw}
    use Grid.{Pos, move, Direction}

    enum Beam {
        case Beam({pos = Pos, direction = Direction})
    }

    instance Eq[Beam] {
        pub def eq(b1: Beam, b2: Beam): Bool = 
            let Beam.Beam({pos=p1, direction=d1}) = b1;
            let Beam.Beam({pos=p2, direction=d2}) = b2;
            p1 == p2 and d1 == d2
    }

    instance Order[Beam] {
        pub def compare(b1: Beam, b2: Beam): Comparison = 
            let Beam.Beam({pos=p1, direction=d1}) = b1;
            let Beam.Beam({pos=p2, direction=d2}) = b2;
            Order.compare((p1, d1), (p2, d2))
    }

    instance ToString[Beam] {
        pub def toString(b: Beam): String = 
            let Beam.Beam({pos=p, direction=d}) = b;
            "(pos=${p}, direction=${d})"
    }

    pub def part1(): Int32 \ {Throw, IO} = 
        run("Day16", countEnergized)

    pub def part2(): Int32 \ {Throw, IO} = 
        run("Day16", maximizeEnergized)

    def run(file: String, part: Grid[Char] -> Int32): Int32 \ {IO, Throw} =
        readInputLines(file) |> Grid.toGrid |> part

    def countEnergized(grid: Grid[Char]): Int32 = {
        let starterBeam = Beam.Beam({pos=(0,0), direction=Direction.E});
        fireBeams(grid, starterBeam)
    }

    def maximizeEnergized(grid: Grid[Char]): Int32 = {
        let height = Grid.height(grid);
        let width = Grid.width(grid);
        let northBeams = List.range(0, width) |> List.map(x -> Beam.Beam({pos=(x,height-1), direction=Direction.N}));
        let eastBeams = List.range(0, height) |> List.map(y -> Beam.Beam({pos=(0,y), direction=Direction.E}));
        let southBeams = List.range(0, width) |> List.map(x -> Beam.Beam({pos=(x,0), direction=Direction.S}));
        let westBeams = List.range(0, height) |> List.map(y -> Beam.Beam({pos=(width-1,y), direction=Direction.W}));
        List#{northBeams, eastBeams, southBeams, westBeams}
            |> List.flatten
            |> List.map(fireBeams(grid))
            |> List.maximum
            |> Option.getWithDefault(0)
    }

    def fireBeams(grid: Grid[Char], starterBeam: Beam): Int32 = region rc {
        let energized = MutSet.empty(rc);
        def moveBeams(beams) = {
            def moveBeam(beam) = {
                let Beam.Beam({pos, direction}) = beam;
                let ch = Grid.at(grid, pos);
                let nextDirections = match (ch, direction) {
                    case ('.', _) => List#{direction}
                    case ('/', Direction.N) => List#{Direction.E}
                    case ('/', Direction.E) => List#{Direction.N}
                    case ('/', Direction.S) => List#{Direction.W}
                    case ('/', Direction.W) => List#{Direction.S}
                    case ('\\', Direction.N) => List#{Direction.W}
                    case ('\\', Direction.E) => List#{Direction.S}
                    case ('\\', Direction.S) => List#{Direction.E}
                    case ('\\', Direction.W) => List#{Direction.N}
                    case ('-', Direction.N) => List#{Direction.W, Direction.E}
                    case ('-', Direction.E) => List#{Direction.E}
                    case ('-', Direction.S) => List#{Direction.W, Direction.E}
                    case ('-', Direction.W) => List#{Direction.W}
                    case ('|', Direction.N) => List#{Direction.N}
                    case ('|', Direction.E) => List#{Direction.N, Direction.S}
                    case ('|', Direction.S) => List#{Direction.S}
                    case ('|', Direction.W) => List#{Direction.N, Direction.S}
                    case _ => bug!("Unhandled")
                };
                nextDirections 
                    |> List.filterMap(d -> move(pos, d, grid) |> Option.map(p -> (d, p)))
                    |> List.map(match (d, p) -> Beam.Beam({direction=d, pos=p}))
            };
            List.flatMap(moveBeam, beams)
                |> List.filter(b -> not MutSet.memberOf(b, energized))
        };
        def loop(beams) = {
            let newBeams = moveBeams(beams);
            if (List.isEmpty(newBeams)) {
                ()
            } else {
                newBeams |> List.forEach(b -> MutSet.add!(b, energized));
                loop(newBeams)
            }
        };
        MutSet.add!(starterBeam, energized);
        loop(List#{starterBeam});
        energized 
            |> MutSet.toSet 
            |> Set.map(match Beam.Beam({pos=pos, direction=_}) -> pos)
            |> Set.size
    }

    @Test 
    pub def testPart1(): Bool =
        assertEq(46, () -> run("Day16Test", countEnergized))

    @Test 
    pub def testPart2(): Bool =
        assertEq(51, () -> run("Day16Test", maximizeEnergized))
}