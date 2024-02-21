val input = adv.readFromFile("05.input")
// val input = List(
//     "seeds: 79 14 55 13",
//     "",
//     "seed-to-soil map:",
//     "50 98 2",
//     "52 50 48",
//     "",
//     "soil-to-fertilizer map:",
//     "0 15 37",
//     "37 52 2",
//     "39 0 15",
//     "",
//     "fertilizer-to-water map:",
//     "49 53 8",
//     "0 11 42",
//     "42 0 7",
//     "57 7 4",
//     "",
//     "water-to-light map:",
//     "88 18 7",
//     "18 25 70",
//     "",
//     "light-to-temperature map:",
//     "45 77 23",
//     "81 45 19",
//     "68 64 13",
//     "",
//     "temperature-to-humidity map:",
//     "0 69 1",
//     "1 0 69",
//     "",
//     "humidity-to-location map:",
//     "60 56 37",
//     "56 93 4"
// )

input.foreach(println)

extension[A] (xs: List[A])
    def splitOn(elem: A): List[List[A]] = 
        xs match
            case Nil => Nil
            case _ =>
                val (firstGroup, nextGroups) = xs.span(_ != elem)
                val rest = 
                    if nextGroups.nonEmpty 
                    then nextGroups.tail.splitOn(elem) 
                    else Nil
                firstGroup :: rest
        
def parseInput(input: List[String]) = 
    val nested = input.splitOn("")
    def parseSeeds(s: String) = s.split(" ").map(_.toLong).toList
    def parseMap(xs: List[String]) = 
        xs.map:
            s => s match
                // case s"$dst $src $len" => (dst.toLong, src.toLong, len.toLong)
                case s"$dst $src $len" => 
                    (src.toLong, 
                    src.toLong + len.toLong - 1,
                    dst.toLong - src.toLong)
                // src_start, src_end, offset
    
    val parsed: List[Any] = nested.map:
        m => m match
            case List(s"seeds: $seedMapString")      => parseSeeds(seedMapString)
            case "seed-to-soil map:" :: t            => parseMap(t)
            case "soil-to-fertilizer map:" :: t      => parseMap(t)
            case "fertilizer-to-water map:" :: t     => parseMap(t)
            case "water-to-light map:" :: t          => parseMap(t)
            case "light-to-temperature map:" :: t    => parseMap(t)
            case "temperature-to-humidity map:" :: t => parseMap(t)
            case "humidity-to-location map:" :: t    => parseMap(t)
            case _ => throw new Exception("unknown type of map")

    val seeds = parsed.head.asInstanceOf[List[Long]]
    val maps  = parsed.tail.asInstanceOf[List[List[(Long, Long, Long)]]]
    
    (seeds, maps)

val (seeds, maps) = parseInput(input)

// Part 1

def applyMap(m: List[(Long, Long, Long)], res: Long) =
    m.find(
        (srcStart, srcEnd, _) => 
            res >= srcStart && res <= srcEnd
    ).map(
        (_, _, offset) => res + offset
    ).getOrElse(res)

seeds.map:
    seed => maps.foldLeft(seed)((res, m) => applyMap(m, res))
.min
