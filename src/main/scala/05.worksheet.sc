import scala.collection.immutable.NumericRange.Exclusive
import scala.collection.immutable.NumericRange.Inclusive
// val almanac = adv.readFromFile("05.input")
val almanac = List(
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4",
)

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

    def stringsToMap(numbers: List[String]) =
        numbers.map(
            s => 
                val List(destination, source, length) = s.split(" ").map(_.toLong).toList
                (source, destination, length)
        )

    val seeds = mapStrings.head match
        case List(s"seeds: $s") => s.split(" ").map(_.toLong).toList
        case _                  => throw Exception("seeds not found")
    
    val maps = mapStrings.tail.map(mapLines =>
        mapLines match
            case "seed-to-soil map:" :: nums            => stringsToMap(nums)
            case "soil-to-fertilizer map:" :: nums      => stringsToMap(nums)
            case "fertilizer-to-water map:" :: nums     => stringsToMap(nums)
            case "water-to-light map:" :: nums          => stringsToMap(nums)
            case "light-to-temperature map:" :: nums    => stringsToMap(nums)
            case "temperature-to-humidity map:" :: nums => stringsToMap(nums)
            case "humidity-to-location map:" :: nums    => stringsToMap(nums)
            case _                                      => throw Exception("unknown type of map")
    )
    
    (seeds, maps)
    
val (seeds, maps) = parseInput(almanac)

def applyMap(m: List[(Long, Long, Long)], x: Long): Long =
    val rangeFound = m.find(ranges => x >= ranges._1 && x < (ranges._1 + ranges._3))

    rangeFound match
        case Some(src, dest, len) => dest + (x - src)
        case None => x

seeds.map(seed => maps.foldLeft(seed)((acc, m) => applyMap(m, acc))).min

// Part 2
extension[A] (l: List[A])
    def groupByTwo: List[(A, A)] =
        l match
            case Nil           => Nil
            case h1 :: h2 :: t => (h1, h2) :: t.groupByTwo
            case _             => throw Exception("odd number of elements")

def parseInputSeedRange(input: List[String]) = 
    val mapStrings = input.splitOn("")

    val seeds = mapStrings.head match
        case List(s"seeds: $s") => s.split(" ").map(_.toLong).toList.groupByTwo.map((start, len) => (start, start + len - 1))
        case _                  => throw Exception("seeds not found")
    
    seeds

val newSeeds = parseInputSeedRange(almanac)

val firstMap = maps.head

// start, end, transformation
firstMap.map(m =>
    (m._1, m._1 + m._3 - 1, m._2 - m._1)
)

def intersection(x1: Long, x2: Long, y1: Long, y2: Long): Option[(Long, Long)] =
    val (x, y) = if x1 < y1 then ((x1, x2), (y1, y2)) else ((y1, y2), (x1, x2)) 

    if y._1 > x._2 then None
    // else (y._1, Math.min(x._2, y._2))
    else
        if 
            x._2 < y._2 
        then 
            // y extends over x
            Some(y._1, x._2)
        else 
            // y ends before x
            Some(y._1, y._2)

intersection(79, 92, 50, 97)

newSeeds
firstMap


// cool, but this doesn't handle the non-intersected fragments of the starting list

// you need to implement the same logic as in the applyMap() function
// add the newly obtained fragments to the list of lists to process
newSeeds.flatMap(seedRange =>
    firstMap.flatMap(m =>
        val mapTransformed = (m._1, m._1 + m._3 - 1, m._2 - m._1)
        intersection(seedRange._1, seedRange._2, mapTransformed._1, mapTransformed._2) match
            // case Some(value) => Some(value._1 + mapTransformed._3, value._2 + mapTransformed._3)
            case Some(value) => Some(value)
            case None => None
    )
)
// applyMap(maps.head, )
// newSeeds.map(seedRange => 
//     seedRange.map(seed => 
//         maps.foldLeft(seed)((acc, m) => applyMap(m, acc))
//     ).min
// ).min


newSeeds
firstMap

def intersectWithComplement(x: (Long, Long), y: (Long, Long)): (Option[(Long, Long)], Option[List[(Long, Long)]]) =
    // four cases

    // no overlap
    if x._2 < y._1 || x._1 > y._2 then (None, Some(List(x)))
    // full overlap (X is in Y)
    else if x._1 >= y._1 && x._2 <= y._2 then (Some(x), None)
    // partial overlap (X to the left of Y)
    else if y._1 > x._1 && y._2 > x._2 then (Some((y._1, x._2)), Some(List((x._1, y._1 - 1))))
    // partial overlap (X to the right of Y)
    else if x._1 > y._1 && x._2 > y._2 then (Some((x._1, y._2)), Some(List((y._2 + 1, x._2))))
    // one-in-the-other (Y is in X)
    else if y._1 > x._1 && y._2 < x._2 then (Some(y._1, y._2), Some(List((x._1, y._1 - 1), (y._2 + 1, x._2))))
    // impossible
    else (None, None)

def applyMapWithComplements(input: List[(Long, Long)], mappings: List[(Long, Long, Long)]) = 
    input match
        case h :: t => mappings.map(m =>
            // start, end, transformation
            val mapTransformed = (m._1, m._1 + m._3 - 1, m._2 - m._1)
            intersectWithComplement(h, (mapTransformed._1, mapTransformed._2))
        )
        case Nil => Nil
    

applyMapWithComplements(newSeeds, firstMap)