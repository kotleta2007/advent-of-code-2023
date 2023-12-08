import scala.collection.immutable.NumericRange.Exclusive
import scala.collection.immutable.NumericRange.Inclusive
val almanac = adv.readFromFile("05.input")
// val almanac = List(
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
//     "56 93 4",
// )

// Part 1
extension[A] (l: List[A])
    def splitOn(x: A): List[List[A]] = 
        if l.isEmpty then Nil
        else if l.head == x then l.tail.splitOn(x)
        else
            val (first, next) = l.span(_ != x)
            first :: next.splitOn(x)

def parseInput(input: List[String]) = 
    val mapStrings = input.splitOn("")

    def seedsToMap(seeds: String): Map[Exclusive[Long], Exclusive[Long]] =
        seeds.split(" +").map(n => (n.toLong until n.toLong+1) -> (0.toLong until 0.toLong)).toMap
    def seedMapToList(seedMap: Map[Exclusive[Long], Exclusive[Long]]) =
        seedMap.map(_._1.start).toList

    def stringsToMap(numbers: List[String]): Map[Exclusive[Long], Exclusive[Long]] =
        numbers.map(
            s => 
                val List(destination, source, length) = s.split(" +").map(_.toLong).toList
                (source until (source + length)) -> (destination until (destination + length))
        ).toMap

    val maps = mapStrings.map(mapLines =>
        mapLines match
            case List(s"seeds: $s")                     => seedsToMap(s)
            case "seed-to-soil map:" :: nums            => stringsToMap(nums)
            case "soil-to-fertilizer map:" :: nums      => stringsToMap(nums)
            case "fertilizer-to-water map:" :: nums     => stringsToMap(nums)
            case "water-to-light map:" :: nums          => stringsToMap(nums)
            case "light-to-temperature map:" :: nums    => stringsToMap(nums)
            case "temperature-to-humidity map:" :: nums => stringsToMap(nums)
            case "humidity-to-location map:" :: nums    => stringsToMap(nums)
            case _                                      => throw Exception("unknown type of map")
    )
    
    (seedMapToList(maps.head), maps.tail)
    
val (seeds, maps) = parseInput(almanac)

def applyMap(m: Map[Exclusive[Long], Exclusive[Long]], x: Long): Long =
    val rangesFound = m.find(ranges => ranges._1.contains(x))

    rangesFound match
        case Some(from, to) => to(from.indexOf(x))
        case None => x

seeds.map(seed => maps.foldLeft(seed)((acc, m) => applyMap(m, acc))).min